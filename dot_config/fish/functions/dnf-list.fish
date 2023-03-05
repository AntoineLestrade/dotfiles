function dnf-list
    dnf --enablerepo develop-jira --showduplicates list $argv
end
