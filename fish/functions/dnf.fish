function dnf-jira
    dnf --enablerepo develop-jira $argv
end

function dnf-list
    dnf --enablerepo develop-jira --showduplicates list $argv
end
