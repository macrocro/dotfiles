export SHELL=/usr/local/bin/zsh
export TERM=xterm-256color

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="gianu"

# Example aliases
alias e='emacsclient'
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
# DISABLE_AUTO_TITLE="true"

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
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin"
# export MANPATH="/usr/local/man:$MANPATH"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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

export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/richmedia/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

function zip_pass() {
    # usage : zip_pass encrypt_zip file/dir
    openssl rand -base64 8 | xargs -Irs ksh -c "zip -r -P rs $1 $2 && echo Password: rs"
}

docker-enter() {
    boot2docker ssh '[ -f /var/lib/boot2docker/nsenter ] || docker run --rm -v /var/lib/boot2docker/:/target jpetazzo/nsenter'
    boot2docker ssh -t sudo /var/lib/boot2docker/docker-enter "$@"
}

ec2-list() {
    aws ec2 describe-instances | jq -r '.Reservations[] | .Instances[] | [.PublicIpAddress,.PrivateIpAddress,.InstanceId,.State.Name,"# "+(.Tags[] | select(.Key == "Name") | .Value // "")] | join("\t")'
}
ec2-start() {
    ec2-list | peco | awk '{print $3}' | xargs aws ec2 start-instances --instance-ids
}

ec2-stop() {
    ec2-list | peco | awk '{print $3}' | xargs aws ec2 stop-instances --instance-ids
}

export PATH=$(brew --prefix)/bin:$PATH

export GOPATH=$HOME
export GOBIN=$HOME/bin
export PATH="/usr/local/sbin:$PATH"

source /usr/local/share/zsh/site-functions/_aws
