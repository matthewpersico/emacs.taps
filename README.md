# .emacs.taps
Git-able customizations for the Exordium emacs system.

When first setting up on a new machine, after pulling down Exordium from
https://github.com/philippe-grenet/exordium.git to establish your local copy of
.emacs.d, clone this repo to establish your customizations:

```bash
cd ~/.emacs.d
mkdir -p taps/$USER
cd taps/$USER
git clone <protocol>:$USER/.emacs.taps .
```

If you are using this code in multiple file systems, you should probably branch
so that you can submit changes without messing up all your other instances. I
use the hostname as the branch name; it's usually unique enough.

```bash
cd ~/.emacs.d/taps/$USER
newbranch=$(hostname)
git checkout -b $newbranch
git push --set-upstream origin $newbranch
```
