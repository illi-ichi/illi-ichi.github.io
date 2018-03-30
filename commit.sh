#! /bin/bash

dir=${0%/*}

message=$(git --git-dir=$dir/../illi-ichi-home-page/.git log -1 --pretty=format:"[%h] %ad %s")

(cd $dir/../illi-ichi-home-page && npx elm make src/Page.elm) && \
    (cd $dir && \
         mv ../illi-ichi-home-page/index.html . && \
         git add . && \
         git commit -m "$message" && \
         git cherry-pick add-ribbon && \
         git push origin master)
