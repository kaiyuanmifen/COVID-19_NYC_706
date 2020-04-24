
import subprocess

def process():
    with open("git_log.tsv", "r") as f:
        for line in f:
            line.rstrip()
            time, commit = line.split()
            subprocess.check_call(f"git --git-dir coronavirus-data/.git worktree add expanded_git_dir/{time}-{commit} {commit}", shell=True)

process()
