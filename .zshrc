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

# alias zshconfig="mate ~/.zshrc"
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

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR="emacsclient"
else
    export EDITOR="emacsclient"
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Docker
docker-id-names() {
    docker ps -q | xargs docker inspect --format='{{.Name}}'
}
alias din='docker-id-names'

docker-exec() {
    docker exec -i -t $@ bash
}
alias de='docker-exec'

alias doco='de $(docker inspect --format="{{.Id}}" $(din|peco|awk "{print \$1}"))'


# AWS
set-aws-default-profile() {
    export AWS_DEFAULT_PROFILE=`egrep '\[.*\]' ~/.aws/credentials | peco | xargs -Iname expr name : '\[\(.*\)\]'`
}

## EC2
ec2-list() {
    aws ec2 describe-instances | jq -r '.Reservations[] | .Instances[] | [.InstanceId,.PublicIpAddress,.PrivateIpAddress,.State.Name,"# "+(.Tags[] | select(.Key == "Name") | .Value // "")] | join("\t")'
}
ec2-start() {
    ec2-list | grep stopped | peco | awk '{print $1}' | xargs aws ec2 start-instances --instance-ids
}

ec2-stop() {
    ec2-list | grep running | peco | awk '{print $1}' | xargs aws ec2 stop-instances --instance-ids
}

ec2-ssh() {
    ssh -i ~/.ssh/docker-registry-hair.pem ec2-user@$(ec2-list | grep running | peco | awk '{print $2}')
}

case ${OSTYPE} in
    darwin*)
	export SHELL=/usr/local/bin/zsh

	export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin"
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

	$(boot2docker shellinit)
        ;;
    linux*)
	export SHELL=/bin/zsh
        # Setting for Linux
        ;;
esac

# Not general-purpose
function backlog() {
    if [[ "$1" =~ "^[0-9]+$" ]]; then
	open "https://richmedia.backlog.jp/view/HAIR-$1"
    else
	open "https://richmedia.backlog.jp/view/$1"
    fi
}

function find-container() {
		docker ps | grep $1 | awk '{print $1}'
}

function zip_pass() {
    # usage : zip_pass encrypt_zip file/dir
    openssl rand -base64 8 | xargs -Irs ksh -c "zip -r -P rs $1 $2 && echo Password: rs"
}

function killpeco() {
		ps aux | peco | awk '{print $2}' | xargs kill -9
}

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# gitignore
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}
function notify() { terminal-notifier -message "$@" }

source ~/.nvm/nvm.sh

function recreate_and_import() {
		target_database=`mysql -uroot -e "show databases\G;" | grep Database | awk '{print $2}' | peco`
		mysql -uroot -e "drop database $target_database; create database $target_database;"
		echo "Recreate database $target_database"

		echo "Import SQL..."
		mysql -uroot $target_database < $@
		echo "Import finished!"
}

# Import private zsh settings
source ~/.zshrc-private
