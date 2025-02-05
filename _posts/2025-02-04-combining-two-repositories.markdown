---
layout:     post
title:      Combining Two Repositories
date:       Tue Feb  4 08:18:45 PM CET 2025
categories: Programming
---

<small style="background-color: #e4e4e4">Comment</small>

# The Problem

I was working for some time on two different repositories, one named
`app-backend` and one named `app-frontend`. After a few weeks of work,
I understood that this was a bad idea.

Instead of two separate repositories, I wanted a single repository
named `app` with two subdirectories `frontend` and `backend`.

The internet is full of articles that describe solutions of variants
of this problem. The following solution is derived from these
articles.

# The Solution

The first step is to download the `git-filter-repo` script from
[newren/git-filter-repo](https://github.com/newren/git-filter-repo/blob/main/)
and move it to some directoy where the shell can find
it. `git-filter-repo` is a standalone Python 3 script that does not
have any other dependency.  (The installation of Python 3 is not
subject of the current little article.)

Now the content of the `app-backend` repository can be moved to the
`backend` subdirectory as follows.

```bash
cd app-backend
mkdir backend
git filter-repo --force --replace-refs delete-no-add --to-subdirectory-filter backend/
```

This moves every file under version control to the `backend` directory
while rewriting all the commits so that they look as if they had
happened in the backend directory right from the beginning.

Similarly, the content of the `app-frontend` directory can be moved to
the `frontend` directory as follows.

```bash
cd app-frontend
mkdir frontend
git filter-repo --force --replace-refs delete-no-add --to-subdirectory-filter frontend/
```

The reason for these two `git-filter-repo` operations is to create two
repositories that do not overlap when actually merging them as shown
below.

```bash
mkdir app
cd app
git init
git checkout -b main
git remote add backend ../app-backend
git remote add frontend ../app-frontend
git fetch --all
git merge backend/main --allow-unrelated-histories
git merge frontend/main --allow-unrelated-histories
```

Looking at the commits using something like `git log -p` shows that
the order of the commits is the expected one and that the commits look
as if they had been taken place in the respective subdirectory.

All that remains to do now is to care of the left-over files that were
not under version control. It may also be a good idea to merge
`.gitignore` files.

There are many recipes showing how to achieve similar results.
However, the ones that I looked at required to move files to backend
and frontend directories, respectively. And this would have introduced
large commits containing all of the code of the repositories. These
commits are avoided by using `git-filter-repo` as described above.
