#!/bin/bash
set -euo pipefail

# Open the firewall ports Sunshine needs for Moonlight clients. The Sunshine rpm
# (steam-packages.sh) ships no firewalld rule, and the default `public` zone
# blocks the stream ports -- so Moonlight can't discover or reach the host until
# these are opened. See fedora/gaming/docs/STREAMING.md.
#
# The rpm install, file caps, udev rule, and user unit are all handled by the
# package itself; only the firewall and one-time web-UI pairing are left, and
# pairing is inherently manual. Run as your normal user; it uses sudo.

# Sunshine's ports (https://docs.lizardbyte.dev/projects/sunshine/latest/about/advanced_usage.html):
#   TCP 47984 47989 47990 48010
#   UDP 47998 47999 48000 48002 48010
tcp_ports=(47984 47989 47990 48010)
udp_ports=(47998 47999 48000 48002 48010)

args=()
for p in "${tcp_ports[@]}"; do args+=(--add-port="$p/tcp"); done
for p in "${udp_ports[@]}"; do args+=(--add-port="$p/udp"); done

# mdns lets Moonlight auto-discover the host; without it clients must add the
# host by IP manually.
args+=(--add-service=mdns)

sudo firewall-cmd --permanent "${args[@]}"
sudo firewall-cmd --reload

echo "Sunshine firewall ports opened."
echo "Verify: sudo firewall-cmd --list-ports"
echo "Then pair: start the stream session (or 'systemctl --user start"
echo "app-dev.lizardbyte.app.Sunshine.service'), open https://localhost:47990 to"
echo "set credentials, and enter the client's PIN there to pair."
