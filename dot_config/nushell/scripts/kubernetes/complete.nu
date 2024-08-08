use argx

use utils.nu *

export def "nu-complete kube ctx" [] {
    let k = (kube-config)
    let cache = $'($env.HOME)/.cache/nu-complete/k8s/($k.path | path basename).json'
    let data = ensure-cache-by-lines $cache $k.path { ||
        let clusters = $k.data | get clusters | select name cluster.server
        let data = $k.data
            | get contexts
            | reduce -f {completion:[], mx_ns: 0, mx_cl: 0} {|x, a|
                let ns = if ($x.context.namespace? | is-empty) { '' } else { $x.context.namespace }
                let max_ns = $ns | str length
                mut cluster = $"($x.context.user)@($clusters | where name == $x.context.cluster | get 'cluster.server')"
                try { $cluster = $"($x.context.user)@($clusters | where name == $x.context.cluster | get 'cluster.server.0')" }
                let max_cl = $cluster | str length
                $a
                | upsert mx_ns (if $max_ns > $a.mx_ns { $max_ns } else $a.mx_ns)
                | upsert mx_cl (if $max_cl > $a.mx_cl { $max_cl } else $a.mx_cl)
                | upsert completion ($a.completion | append {value: $x.name, ns: $ns, cluster: $cluster})
            }
        {completion: $data.completion, max: {ns: $data.mx_ns, cluster: $data.mx_cl}}
    }

    $data.completion | each {|x|
        let ns = $x.ns | fill -a l -w $data.max.ns -c ' '
        let cl = $x.cluster | fill -a l -w $data.max.cluster -c ' '
        {value: $x.value, description: $"\t($ns) ($cl)"}
    }
}
