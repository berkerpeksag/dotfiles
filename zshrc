if [ -f '/Users/berker.peksag/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/berker.peksag/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/Users/berker.peksag/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/berker.peksag/google-cloud-sdk/completion.zsh.inc'; fi

export IPRAY_NAMESPACE="berker"

use-west2() {
    gcloud container clusters get-credentials --project iprally-ai-dev mlops-dev --region europe-west2
    kubectl config use-context gke_iprally-ai-dev_europe-west2_mlops-dev
    kubectl config set-context --current --namespace=$IPRAY_NAMESPACE
}

use-west4() {
    gcloud container clusters get-credentials --project iprally-ai-dev mlops-west4-dev --region europe-west4
    kubectl config use-context gke_iprally-ai-dev_europe-west4_mlops-west4-dev
    kubectl config set-context --current --namespace=$IPRAY_NAMESPACE
}

use-asia-ne3() {
    gcloud container clusters get-credentials mlops-asia-ne3-dev --region asia-northeast3 --project iprally-ai-dev
    kubectl config use-context gke_iprally-ai-dev_asia-northeast3_mlops-asia-ne3-dev
    kubectl config set-context --current --namespace=$IPRAY_NAMESPACE
}

mkd() {
    mkdir -p "$@" && cd "$@"
}

venv() {
    source .venv/bin/activate
}

dvenv() {
    deactivate
}

parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

PROMPT='%B%m%~%b$(parse_git_branch) %# '

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export PATH=/Users/berker.peksag/projects/gino:/Applications/Postgres.app/Contents/Versions/latest/bin:$HOME/projects/workstation/bin:/opt/homebrew/opt/node@20/bin:$PATH

if [ -f ~/.zsh_aliases ]; then
    . ~/.zsh_aliases
fi
