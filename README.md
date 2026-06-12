# emacs.taps
Git-able customizations for the Exordium emacs system.

When setting up on a new machine

* Clone Exordium.

```bash
git clone https://github.com/emacs-exordium/exordium.git ~/.emacs.d
```

* Start emacs.
Let it initialize the packages in Exordium, and do all the asyc native compilation stuff. Bring up the `*Async-native-compile-log*` buffer and wait until it is done scrolling by. Then exit emacs and continue.

* Clone *this* repo to establish your customizations; **WARNING:** Do not execute
this step before starting emacs once.

```bash
cd ~/.emacs.d
mkdir -p taps/$USER
cd taps/$USER
git clone git@github.com:matthewpersico/emacs.taps .
cd ~/.emacs.d
rm -f emacs-custom.el
ln -s taps/$USER/emacs-custom.el .
```

* If you are using this code in multiple file systems, you should probably
branch so that you can submit changes without messing up all your other
instances. I use the hostname as the branch name; it's usually unique enough.

```bash
cd ~/.emacs.d/taps/$USER
newbranch=$(hostname)
git checkout -b $newbranch
git push --set-upstream origin $newbranch
```

Of course, the instructions only apply if you are directly accessing the GitHub
repos. If using GHE, see `README-GHE.md` in the `personal` repo if you need to
keep a GHE copy.
