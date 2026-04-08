if [[ -d "$HOME/infer" ]] || [[ -d "$HOME/devserver" ]]; then
  export PATH="$HOME/infer/infer/bin:$HOME/infer/facebook/dependencies/bin:$HOME/devserver/scripts:$PATH"
  export BUILD_MODE=default
  export MANPATH="$HOME/infer/infer/man":$MANPATH

  proxy() {
    local p=fwdproxy:8080
    HTTPS_PROXY=$p HTTP_PROXY=$p https_proxy=$p http_proxy=$p "$@"
  }

  proxy_py() {
    local p=http://fwdproxy:8080
    HTTPS_PROXY=$p HTTP_PROXY=$p https_proxy=$p http_proxy=$p "$@"
  }
fi
