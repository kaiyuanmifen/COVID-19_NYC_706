
git -C coronavirus-data/ log --pretty=format:"%at%x09%h" > ./git_log.tsv

rm -rf expanded_git_dir
mkdir expanded_git_dir

function process {
	git --git-dir coronavirus-data/ worktree add expanded_git_dir/"$1-$2" $2
}

export -f process

cat git_log.tsv | xargs -L 1  bash -c 'process $@'



