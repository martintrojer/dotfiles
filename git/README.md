# Git

Shared Git defaults live in `.gitconfig.shared`.

## GitHub + Forgejo mirrors

Repos are mirrored verbatim:

- GitHub: SSH fetch/push via `origin`
- Forgejo: SSH push via `bubba:3022`

Forgejo only works on home Wi-Fi/LAN.

### Configure a repo

Git repo, for a repo named `REPO`:

```bash
git remote set-url origin git@github.com:martintrojer/REPO.git
git config --unset-all remote.origin.pushurl 2>/dev/null || true
git remote set-url --add --push origin git@github.com:martintrojer/REPO.git
git remote set-url --add --push origin ssh://git@bubba:3022/martintrojer/REPO.git
git remote add bubba ssh://git@bubba:3022/martintrojer/REPO.git
```

jj repo:

```bash
jj git remote set-url --fetch git@github.com:martintrojer/REPO.git --push git@github.com:martintrojer/REPO.git origin
jj git remote add --push-url ssh://git@bubba:3022/martintrojer/REPO.git bubba ssh://git@bubba:3022/martintrojer/REPO.git
```

Verify:

```bash
git remote -v
jj git remote list    # in jj repos
```

### Push current repo

Git repos can fan out `origin.pushurl` with plain:

```bash
git push
```

jj does not push to multiple `origin.pushurl` values, so push both remotes explicitly:

```bash
jj git push --remote origin --all
jj git push --remote bubba --all
```

### Audit Bubba mirror drift

Use GitHub as the repo inventory and compare default-branch heads against Bubba:

```bash
python3 - <<'PY'
import json
import subprocess
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed

OWNER = "martintrojer"
LIMIT = "200"
TIMEOUT = 10

repos = json.loads(
    subprocess.check_output(
        [
            "gh",
            "repo",
            "list",
            OWNER,
            "--limit",
            LIMIT,
            "--json",
            "name,isFork,isArchived,defaultBranchRef,sshUrl,visibility,pushedAt",
        ],
        text=True,
    )
)
repos = [repo for repo in repos if not repo.get("isFork") and not repo.get("isArchived")]


def default_branch(repo):
    ref = repo.get("defaultBranchRef") or {}
    return ref.get("name") or "main"


def ls_remote(url, branch):
    try:
        out = subprocess.check_output(
            ["git", "ls-remote", "--heads", url, branch],
            text=True,
            stderr=subprocess.DEVNULL,
            timeout=TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return "timeout", None
    except subprocess.CalledProcessError:
        return "error", None
    line = out.strip().splitlines()[0] if out.strip() else ""
    if not line:
        return "missing-branch", None
    return "ok", line.split()[0]


def check(repo):
    branch = default_branch(repo)
    github_url = repo["sshUrl"]
    bubba_url = f"ssh://git@bubba:3022/{OWNER}/{repo['name']}.git"
    github_status, github_sha = ls_remote(github_url, branch)
    bubba_status, bubba_sha = ls_remote(bubba_url, branch)
    if github_status != "ok":
        state = f"github-{github_status}"
    elif bubba_status != "ok":
        state = f"bubba-{bubba_status}"
    elif github_sha == bubba_sha:
        state = "in-sync"
    else:
        state = "bubba-behind-or-diverged"
    return {
        "name": repo["name"],
        "branch": branch,
        "github_sha": github_sha,
        "bubba_sha": bubba_sha,
        "state": state,
    }


results = []
with ThreadPoolExecutor(max_workers=8) as executor:
    futures = [executor.submit(check, repo) for repo in repos]
    for future in as_completed(futures):
        results.append(future.result())

order = {
    "bubba-behind-or-diverged": 0,
    "bubba-missing-branch": 1,
    "bubba-error": 2,
    "bubba-timeout": 3,
    "in-sync": 9,
}
results.sort(key=lambda row: (order.get(row["state"], 5), row["name"]))

print("Repos not in sync with bubba:")
print(f"{'state':<24} {'repo':<30} {'branch':<10} {'github':<12} {'bubba':<12}")
for row in results:
    if row["state"] == "in-sync":
        continue
    print(
        f"{row['state']:<24} {row['name']:<30} {row['branch']:<10} "
        f"{(row['github_sha'] or '-')[:12]:<12} {(row['bubba_sha'] or '-')[:12]:<12}"
    )

print("\nSummary:")
for state, count in Counter(row["state"] for row in results).most_common():
    print(f"{state}: {count}")
PY
```

### Sync a repo from GitHub to Bubba

For fast-forwardable repos, no local clone is needed:

```bash
repo=REPO
branch=main
workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

git init --bare "$workdir/$repo.git"
git -C "$workdir/$repo.git" remote add github "git@github.com:martintrojer/$repo.git"
git -C "$workdir/$repo.git" remote add bubba "ssh://git@bubba:3022/martintrojer/$repo.git"
git -C "$workdir/$repo.git" fetch github "+refs/heads/$branch:refs/remotes/github/$branch"
git -C "$workdir/$repo.git" push bubba "refs/remotes/github/$branch:refs/heads/$branch"
```

If Bubba rejects with `fetch first`, the histories diverged or Bubba has commits GitHub does not. Inspect before force-pushing.
