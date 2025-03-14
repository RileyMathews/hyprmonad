ghciwatch:
    ghciwatch --command "stack repl --no-load --flag hyprmonad:dev" --watch app --watch src --error-file=ghcid.txt

# use cabal build/run for running the project normally
