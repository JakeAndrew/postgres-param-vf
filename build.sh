#!/usr/bin/env bash

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/Yesod things like run tests and run the
  devel site.

  It uses the shake build system to make sure a dev/test database is
  running.

  CMD can be:

      devel
      test
      psql
      shell

  'test' and 'psql' both accept any additional options native to those
  commands.

  'shell' is an advanced command, which sets up the Postgres and stack
  environments and spawns a new shell.
EOF

# Runs the development site:
run_devel () {

    # Move into the website subdirectory:
    cd "$projectRoot"/website

    # If the Nix shell variable is either empty or not set, then...
    if [ -z "$IN_NIX_SHELL" ]; then

        # ...make sure we have the "yesod" command (download & build if needed):
        # (The package name is "yesod-bin", the executable is just "yesod".)
        stack build yesod-bin && # Skips if already built.

        # Start the devel site using Yesod via Stack, now knowing we have Yesod:
        exec stack exec yesod devel
    else
        # Otherwise, we should be in an environment that makes "yesod" directly
        # available to us: no need for "stack" or building Yesod ourselves.
        exec yesod devel
    fi

    cd - # Now we go back to where we were before we exit the subroutine.
}

# Sets _d_ata_b_ase _env_ironment variables and starts the server:
dbenv () {
    stack exec sdb start  # Start the database server.

    # Set environment variables used by Postgres. Method: execute the "export"
    # commands outputted by "sdb env". Having these set is useful for the "psql"
    # and "shell" commands of this script.
    source <(stack exec sdb env)
}

main () {

    # If no arguments are given, assume the "devel" arg (command) by default:
    #
    # If the "first arg", "$1", is an empty string or not defined, meaning that
    # there is no first arg, and therefore, there are no args, then...
    if [ -z "$1" ]; then
        CMD=devel #...set CMD (command) to "devel".
    else
        # Otherwise, set CMD to our first arg because we *were* given an arg:
        CMD="$1"

        # Next, shift the args over. The second arg is now the first, the third
        # now the second, and so on. The original first is now CMD (see above).
        shift
    fi

    # Move into the project root, specified in absolute form. The "stack build"
    # commands below need to be ran from within the project. We use absolute
    # form because we are suspicious of buggy behavior induced by relative paths.
    cd "$projectRoot"


    # Configure local Stripe keys for shell, devel, and test:
    #
    # (Recall that the current directory is the project root, due to the "cd"
    # above.)
    # If the .stripe_keys file exists in the current directory, then ...
    if [ -e .stripe_keys ]; then

        # ... load the keys as environment variables. Method: execute the export
        # commands in the file.
        source .stripe_keys;

    # Otherwise, print a friendly reminder:
    else
        echo
        echo "Friendly reminder (not an error): there is no \".stripe_keys\" file"
        echo "in the project root. You may wish to consult BUILD.md about that."
        echo
    fi

    # Note the "--only-dependencies" argument. The Snowdrift package itself is
    # not being compiled here, but its dependecies are, including the local
    # ones. Except it doesn't compile anything already built.
    stack build --flag Snowdrift:library-only --only-dependencies --install-ghc Snowdrift:test &&

    # We also want to build admin-tools, which contains the sdb program.
    # The above command does not build it because it is not a dependency of
    # Snowdrift. Appending "admin-tools" to the above wouldn't get it built
    # either, because the above only compiles the dependencies of the specified
    # packages. Note we want to build this package before running dbenv below,
    # because dbenv needs sdb, and sdb is furnished by this package.
    stack build admin-tools &&

    # Start the database and export some environment variables:
    dbenv &&

    # Now we just act in accordance to the command (CMD) we've been given
    # (the argument passed to this script or assumed by default):
    case "$CMD" in
        devel)          # Recall that we assume this one by default.
            run_devel   # (Defaulting is the first thing done in main.)
            ;;
        test)
            exec stack test --flag Snowdrift:library-only --fast "$@"
            ;;
        psql)     # Here's a good reason for us setting those env vars in dbenv.
            exec psql "$@"
            ;;
        shell)    # And another good reason for having set those env vars.
            exec stack exec bash
            ;;
        *)        # The star is the catch-all case. If we got here, the arg
            echo "$usage" # is illegal, so print the usage text.
            ;;
    esac
}

# Establish project root, in absolute form:
#
# `dirname "$0"` gets the path of the script's containing directory, which is
# also the project root. Notice this method works no matter where the script was
# launched from (PWD).
scriptDirAsGiven="`dirname "$0"`"
#
# Now we use `readlink -f` to ensure the path is absolute:
projectRoot="`readlink -f "$scriptDirAsGiven"`"

# Now launch main, passing in all our args:
main "$@"
