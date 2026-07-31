#!/bin/bash
set -euo pipefail

# Restrict Sunshine to this host's wired LAN. Rules are installed in an
# explicit zone and accept only the configured source network; the ambient
# default firewalld zone is never used.
#
# Override these for a different host/network:
#   SUNSHINE_FIREWALL_ZONE=home
#   SUNSHINE_FIREWALL_INTERFACE=enp1s0
#   SUNSHINE_LAN_CIDR=10.0.0.0/24

zone=${SUNSHINE_FIREWALL_ZONE:-public}
interface=${SUNSHINE_FIREWALL_INTERFACE:-enp9s0}
lan_cidr=${SUNSHINE_LAN_CIDR:-192.168.0.0/16}
action=${1:-apply}

case "$action" in
  apply | --verify | --revert) ;;
  *)
    echo "usage: $0 [apply|--verify|--revert]" >&2
    exit 2
    ;;
esac

# Sunshine's ports (https://docs.lizardbyte.dev/projects/sunshine/latest/about/advanced_usage.html):
#   TCP 47984 47989 47990 48010
#   UDP 47998 47999 48000 48002 48010
tcp_ports=(47984 47989 47990 48010)
udp_ports=(47998 47999 48000 48002 48010)
rules=()
for port in "${tcp_ports[@]}"; do
  rules+=("rule family=\"ipv4\" source address=\"$lan_cidr\" port port=\"$port\" protocol=\"tcp\" accept")
done
for port in "${udp_ports[@]}"; do
  rules+=("rule family=\"ipv4\" source address=\"$lan_cidr\" port port=\"$port\" protocol=\"udp\" accept")
done
# mDNS lets Moonlight discover the host without exposing discovery beyond LAN.
rules+=("rule family=\"ipv4\" source address=\"$lan_cidr\" service name=\"mdns\" accept")

firewall() {
  sudo firewall-cmd "$@"
}

check_interface_zone() {
  local actual_zone
  actual_zone=$(firewall --get-zone-of-interface="$interface" || true)
  if [[ "$actual_zone" != "$zone" ]]; then
    echo "error: $interface is in firewalld zone '${actual_zone:-none}', expected '$zone'." >&2
    echo "Set SUNSHINE_FIREWALL_ZONE to its trusted LAN zone, or assign the connection explicitly." >&2
    return 1
  fi
}

verify() {
  local rule scope status=0
  local scope_args=()

  check_interface_zone || status=1
  for scope in runtime permanent; do
    scope_args=()
    if [[ "$scope" == permanent ]]; then
      scope_args=(--permanent)
    fi
    for rule in "${rules[@]}"; do
      firewall "${scope_args[@]}" --zone="$zone" --query-rich-rule="$rule" >/dev/null || {
        echo "missing $scope rule: $rule" >&2
        status=1
      }
    done
  done

  if ((status == 0)); then
    echo "Sunshine is restricted to $lan_cidr on $interface ($zone zone)."
  fi
  return "$status"
}

if [[ "$action" == --verify ]]; then
  verify
  exit
fi

if [[ "$action" == --revert ]]; then
  for rule in "${rules[@]}"; do
    if firewall --permanent --zone="$zone" --query-rich-rule="$rule" >/dev/null; then
      firewall --permanent --zone="$zone" --remove-rich-rule="$rule"
    fi
  done
  firewall --reload
  echo "Removed Sunshine's LAN-scoped firewall rules from the $zone zone."
  exit
fi

check_interface_zone

# Remove broad rules left by older versions of this script before adding their
# LAN-scoped replacements.
for port in "${tcp_ports[@]}"; do
  if firewall --permanent --zone="$zone" --query-port="$port/tcp" >/dev/null; then
    firewall --permanent --zone="$zone" --remove-port="$port/tcp"
  fi
done
for port in "${udp_ports[@]}"; do
  if firewall --permanent --zone="$zone" --query-port="$port/udp" >/dev/null; then
    firewall --permanent --zone="$zone" --remove-port="$port/udp"
  fi
done
if firewall --permanent --zone="$zone" --query-service=mdns >/dev/null; then
  firewall --permanent --zone="$zone" --remove-service=mdns
fi

for rule in "${rules[@]}"; do
  if ! firewall --permanent --zone="$zone" --query-rich-rule="$rule" >/dev/null; then
    firewall --permanent --zone="$zone" --add-rich-rule="$rule"
  fi
done
firewall --reload

verify
echo "Then pair in Sunshine's web UI at https://localhost:47990."
