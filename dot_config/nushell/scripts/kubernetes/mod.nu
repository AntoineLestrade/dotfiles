
### ctx
export def "kube-config" [] {
    let file = if ($env.KUBECONFIG? | is-empty) { $"($env.HOME)/.kube/config" } else { $env.KUBECONFIG }
    { path: $file, data: (cat $file | from yaml) }
}


use complete.nu *

# kubectl change context
export def kcc [context: string@"nu-complete kube ctx"] {
    kubectl config use-context $context
}
