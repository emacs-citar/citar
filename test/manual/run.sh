#!/usr/bin/env bash
# adapted from https://github.com/raxod502/selectrum/tree/master/test
/usr/bin/env \
    emacs -q --no-site-file --no-site-lisp --no-splash -l install.el -l citar.el
