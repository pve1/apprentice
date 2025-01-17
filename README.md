# Apprentice Quickstart

The purpose of Apprentice is to passively provide helpful information
to the user based on the current state of a Lisp mode Emacs buffer.
For instance, if the point is on a symbol, then the apprentice buffer
might show the output of `cl:describe` for that symbol. Much more is
possible, as demonstrated by the included example apprentice.

-   Download and install the dependencies
    -   <https://github.com/pve1/extensible-inferred-system>
    -   <https://github.com/pve1/capitalized-export>

-   Load `apprentice.el` in Emacs.
-   Load the `apprentice/example-apprentice` system in Lisp.
-   While not strictly necessary, some useful features (notably the
    suggestions in the example apprentice) require
    slime-enable-evaluate-in-emacs to be non-nil.
-   Bind the following keys:

    (define-key lisp-mode-map (kbd "C-c z") 'apprentice-describe)
    (define-key lisp-mode-map (kbd "C-c Z") 'apprentice-describe-form)

-   Open a lisp buffer and press `C-c z` to bring up the apprentice
    buffer.
    -   Move the point around to see what it does.
    -   Try typing something at the toplevel to see some
        suggestions. Location matters, e.g. at the beginning of a buffer
        it will suggest a defpackage form. When creating slots for a
        class, it will show slot-related suggestions.
    -   Things highlighted with colors in the apprentice buffer are clickable
        with RET.
-   NOTE: `apprentice-describe-form` will repeatedly evaluate the form
    behind the point, so don't use it on (delete-file &#x2026;) or other
    dangerous forms. Try it on `(random 10)` to get an idea.

# Example apprentice demos

## Creating a package

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/create-package-and-class.mp4)]

## Exporting symbols

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/exporting-symbols.mp4)]

## Using the overview

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/overview.mp4)]

## Toggling package qualifiers

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/using-the-qual-button.mp4)]

## Grepping

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/mentions-and-replacing.mp4)]

## Monitoring forms

[![Demo](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/monitor-form.mp4)]
