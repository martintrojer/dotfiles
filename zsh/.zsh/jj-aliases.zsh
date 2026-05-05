# Cherry-picked from oh-my-zsh jj plugin. Only the aliases that
# actually appeared in zsh history (>= 5 uses; see CLAUDE.md
# "Audit usage with shell history" rule). OMZ ships ~35 jj aliases;
# we use 15.

alias jja='jj abandon'
alias jjb='jj bookmark'
alias jjd='jj diff'
alias jjdmsg='jj desc --message'
alias jje='jj edit'
alias jjgp='jj git push'
alias jjl='jj log'
alias jjla='jj log -r "all()"'
alias jjn='jj new'
alias jjrb='jj rebase'
alias jjrbm='jj rebase-main'  # custom jj alias (jj/.config/jj/config.toml): rebase children of main@origin onto main
alias jjs='jj show'
alias jjsq='jj squash'
alias jjst='jj status'
alias jjt='jj tug'  # custom jj alias (jj/.config/jj/config.toml): move nearest bookmark to @
