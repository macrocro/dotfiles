export TERM=xterm-256color

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="gianu"

# aliases
alias e='emacsclient -nw'
alias tigs="tig status"
alias tiga="tig --all"
alias -g B='`git branch -a | peco --prompt "GIT BRANCH>" | head -n 1 | sed -e "s/^\*\s*//g"`'
alias ll='ls -al'

# aliases git
alias g='git'
alias gs='git status'
alias gp='git pull'
alias gpu='git push'
alias gr='git remote'

# ALIAS zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# enhancd
source ~/.enhancd/init.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR="emacsclient"
else
    export EDITOR="emacsclient"
fi

# FZF
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'

export GLOBAL_IP_URL="http://httpbin.org/ip"


# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Emacs

alias restart-emacs="ps aux | grep emacs | grep daemon | awk '{print \$2}' | xargs kill && emacs --daemon"

# Docker
docker-id-names() {
		docker ps --format '{{.Names}}'
}
alias din='docker-id-names'

docker-exec() {
    docker exec -i -t $@ bash
}
alias de='docker-exec'
alias doco='de $(docker inspect --format="{{.Id}}" $(din|peco|awk "{print \$1}"))'
alias doca='docker attach $(docker inspect --format="{{.Id}}" $(din|peco|awk "{print \$1}"))'
alias dcew='noti docker-compose exec web $1'
alias dcrw='noti docker-compose run web $1'
alias dcer='noti docker-compose exec rails $1'
alias finde='e $(fzf)'

update-docker-hosts(){
	# clear existing *.docker.local entries from /etc/hosts
	sudo sed -i '' '/\.docker\.local$/d' /etc/hosts

	# iterate over each machine
	docker-machine ls | tail -n +2 | awk '{print $1}' \
	| while read -r MACHINE; do
		MACHINE_IP="$(docker-machine ip ${MACHINE} 2>/dev/null)"
		[[ -n $MACHINE_IP ]] && sudo /bin/bash -c "echo \"${MACHINE_IP}	${MACHINE}.docker.local\" >> /etc/hosts"
		export no_proxy=$no_proxy,$MACHINE_IP
	done
}

# AWS
set-aws-default-profile() {
    if [[ "$1" =~ "^[a-zA-Z0-9]+$" ]]; then
        export AWS_DEFAULT_PROFILE=`egrep '\[.*\]' ~/.aws/credentials | grep $1 | xargs -Iname expr name : '\[\(.*\)\]'`
    else
        export AWS_DEFAULT_PROFILE=`egrep '\[.*\]' ~/.aws/credentials | peco | xargs -Iname expr name : '\[\(.*\)\]'`
    fi
}

## EC2
ec2-list() {
		aws ec2 describe-instances | jq -r '.Reservations[] | .Instances[] | [.InstanceId,.PublicIpAddress,.PrivateIpAddress,"# "+.State.Name, (.Tags[]? | select(.Key == "Name")).Value] | join("\t")'
}
ec2-start() {
    ec2-list | grep stopped | peco | awk '{print $1}' | xargs aws ec2 start-instances --instance-ids
}

ec2-stop() {
    ec2-list | grep running | peco | awk '{print $1}' | xargs aws ec2 stop-instances --instance-ids
}

open-security-group() {
    MYIP=`curl -s $GLOBAL_IP_URL | jq -r .origin`

    echo "** authorize security group for SG: $1, IP: $MYIP, Port: $2 **"
    aws ec2 authorize-security-group-ingress --group-id $1 --protocol tcp --port $2 --cidr $MYIP/32
}

close-security-group() {
    MYIP=`curl -s $GLOBAL_IP_URL | jq -r .origin`

    aws ec2 revoke-security-group-ingress --group-id $1 --protocol tcp --port $2 --cidr $MYIP/32
    echo "** revoke security group for SG: $1, IP: $MYIP, Port: $2 **"
}



case ${OSTYPE} in
    darwin*)
	export SHELL=/usr/local/bin/zsh

	export PATH="/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/bin:/opt/X11/bin"
	# export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin"
	# export MANPATH="/usr/local/man:$MANPATH"

  # Setting for Mac OS
	export PATH=$(brew --prefix)/bin:$PATH

	export GOPATH=$HOME
	export GOBIN=$HOME/bin
	# Golang bin Path
	export PATH="$HOME/bin:$PATH"
	export PATH="/usr/local/sbin:$PATH"

	source /usr/local/share/zsh/site-functions/_aws

	alias tmux-copy='tmux save-buffer - | reattach-to-user-namespace pbcopy'
        ;;
    linux*)
	export SHELL=/bin/zsh
        # Setting for Linux
        ;;
esac

find-container() {
		docker ps | grep $1 | awk '{print $1}'
}

zip_pass() {
    # usage : zip_pass encrypt_zip file/dir
    openssl rand -base64 8 | xargs -Irs ksh -c "zip -r -P rs $1 $2 && echo Password: rs"
}

killpeco() {
		ps aux | peco | awk '{print $2}' | xargs kill -9
}

## rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

## phpenv
# export PATH="$HOME/.anyenv/bin:$PATH"
# eval "$(anyenv init -)"

# gitignore
gi() { curl -L -s https://www.gitignore.io/api/$@ ;}


notify() { terminal-notifier -message "$@" }

export NVM_DIR="$HOME/.nvm"
. "/usr/local/opt/nvm/nvm.sh"

recreate_and_import() {
		target_database=`mysql -uroot -e "show databases\G;" | grep Database | awk '{print $2}' | peco`
		mysql -uroot -e "drop database $target_database; create database $target_database;"
		echo "Recreate database $target_database"

		echo "Import SQL..."
		mysql -uroot $target_database < $@
		echo "Import finished!"
}

dump_mysql_database() {
		target_database=`mysql -uroot -e "show databases\G;" | grep Database | awk '{print $2}' | peco`
		mysqldump -uroot $target_database
}

add_br_git_status () {
		git status -s | awk '{print $2}' | xargs -Ifilename sh -c 'echo "\n" >> filename'
}

find-code () {
		grep -r $1 $2 | grep -v ".min."
}

ec () {
		emacsclient -nw -e "(find-file \"$1\")";
}

open-find-code () {
		grep -r $1 $2 | grep -v ".min." | peco | sed -e 's/\(.*\)\:.*/\1/g' | xargs -Ifilename sh -c "emacsclient -nw filename";
}

hub-pull-request () {
		organization_name=${ORGANIZATION_NAME:-"wakeapp-inc"}
		hub pull-request -b $organization_name/$REPOSITORY_NAME:$1 -h $organization_name/$REPOSITORY_NAME:$2
}

# http://interprism.hatenablog.com/entry/peco-zsh-history
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | eval $tac | awk '!a[$0]++' | peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    # zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

# direnv : https://github.com/direnv/direnv
eval "$(direnv hook zsh)"

# Import private zsh settings
source ~/.zshrc-private

# https://github.com/emacs-jp/emacs-jp.github.com/issues/18#issuecomment-13697644
# create emacs env file
perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH  VIRTUAL_ENV > ~/.emacs.d/shellenv.el

export PATH="$HOME/.yarn/bin:$PATH"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="/usr/local/opt/python/libexec/bin:$PATH"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/youki/.nvm/versions/node/v6.10.0/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/youki/.nvm/versions/node/v6.10.0/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/youki/.nvm/versions/node/v6.10.0/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/youki/.nvm/versions/node/v6.10.0/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

