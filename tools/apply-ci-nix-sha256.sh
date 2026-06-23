#!/usr/bin/env bash
set -euo pipefail

usage() {
    echo "Usage: $0 [github-job-url|job-id|log-file|-|--self-test]" >&2
}

fetch_log() {
    case "${1:--}" in
        -)
            cat
            ;;
        *github.com*/actions/runs/*/job/*)
            gh api "repos/lamdu/lamdu/actions/jobs/${1##*/}/logs"
            ;;
        *[!0-9]*)
            cat "$1"
            ;;
        *)
            gh api "repos/lamdu/lamdu/actions/jobs/$1/logs"
            ;;
    esac
}

apply_log() {
    local stack_yaml=$1
    local log_file=$2

    read -r pkg old_hash new_hash < <(
        perl -0ne '
            if (m{fixed-output derivation '\''[^'\'']*/[0-9a-z]+-(.+)-[0-9a-f]{6,}\.drv'\''.*?specified:\s*(sha256-\S+).*?got:\s*(sha256-\S+)}s) {
                print "$1 $2 $3\n";
                exit;
            }
        ' "$log_file"
    )

    if [[ -z "${pkg:-}" ]]; then
        echo "No fixed-output hash mismatch found" >&2
        return 1
    fi

    if PKG=$pkg NEW_HASH=$new_hash perl -0ne '
        my $pkg = quotemeta $ENV{PKG};
        my $new = $ENV{NEW_HASH};
        if (m{- github:\s+\S+/$pkg\n(?:[^\n]*\n)*?\s*# nix-sha256:\s*\Q$new\E}) {
            exit 0;
        }
        exit 1;
    ' "$stack_yaml"; then
        echo "$pkg: already $new_hash"
        return
    fi

    PKG=$pkg OLD_HASH=$old_hash NEW_HASH=$new_hash perl -0pi -e '
        my $pkg = quotemeta $ENV{PKG};
        my $old = quotemeta $ENV{OLD_HASH};
        my $new = $ENV{NEW_HASH};
        my $n = s{(- github:\s+\S+/$pkg\n(?:[^\n]*\n)*?\s*# nix-sha256:\s*)$old}{$1$new};
        die "Did not find $ENV{PKG} with $ENV{OLD_HASH} in stack.yaml\n" unless $n == 1;
    ' "$stack_yaml"

    echo "$pkg: $old_hash -> $new_hash"
}

self_test() {
    local dir
    dir=$(mktemp -d)
    trap 'rm -rf "$dir"' RETURN
    cat > "$dir/stack.yaml" <<'EOF'
extra-deps:
  - github: lamdu/bindings-GLFW
    commit: 28c1e5444367dd06f460af6604de96e4d795b019
    # nix-sha256: sha256-old=
EOF
    cat > "$dir/log" <<'EOF'
error: hash mismatch in fixed-output derivation '/nix/store/x-bindings-GLFW-28c1e54.drv':
         specified: sha256-old=
            got:    sha256-new=
EOF
    apply_log "$dir/stack.yaml" "$dir/log" >/dev/null
    apply_log "$dir/stack.yaml" "$dir/log" >/dev/null
    grep -q 'sha256-new=' "$dir/stack.yaml"
}

case "${1:-}" in
    --help|-h)
        usage
        ;;
    --self-test)
        self_test
        ;;
    *)
        tmp=$(mktemp)
        trap 'rm -f "$tmp"' EXIT
        fetch_log "${1:--}" > "$tmp"
        apply_log stack.yaml "$tmp"
        ;;
esac
