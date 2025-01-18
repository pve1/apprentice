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

[qwe](https://github.com/pve1/apprentice/raw/refs/heads/assets/create-package-and-class.mp4)

![foo](https://github.com/pve1/apprentice/blob/assets/create-package-and-class.mp4)

[test](https://github.com/pve1/apprentice-videos/raw/refs/heads/main/create-package-and-class.mp4)

## Creating a package


https://github.com/user-attachments/assets/70aa3e7b-65b4-4a69-aeaf-58c8086a7ed4


## Exporting symbols


https://github.com/user-attachments/assets/afcc93db-48db-49cf-8e8e-b75829b08c5d


## Using the overview


https://github.com/user-attachments/assets/fe1c5fd7-2c9f-4c72-8ba4-f8fa6310d9f5


## Toggling package qualifiers


https://github.com/user-attachments/assets/ef5d2eb4-251f-4faa-9eb6-7b261f13902c


## Grepping


https://github.com/user-attachments/assets/ff6be0ee-4c2b-4939-a13e-e3f441358625


## Monitoring forms


https://github.com/user-attachments/assets/d366a262-84ad-41dd-ad1b-f873cb541bd3

