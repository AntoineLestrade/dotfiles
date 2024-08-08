export def ensure-cache-by-lines [cache path action] {
    let ls = do -i { open $path | lines | length }
    if ($ls | is-empty) { return false }
    let lc = do -i { open $cache | get lines}
    if not (($cache | path exists) and ($lc | is-not-empty) and ($ls == $lc)) {
        mkdir ($cache | path dirname)
        {
            lines: $ls
            payload: (do $action)
        } | save -f $cache
    }
    (open $cache).payload
}


export def `kcache flush` [] {
    rm -rf ~/.cache/nu-complete/k8s/
    nu-complete kube ctx
    rm -rf ~/.cache/nu-complete/k8s-api-resources/
}