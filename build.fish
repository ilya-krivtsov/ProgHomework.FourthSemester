#!/usr/bin/env fish

if test (count $argv) -ne 1
    echo "only one argument 'build' or 'test' should be used"
    return 1
end

set command "$argv[1]"

if test "$command" = build
    for sln in (find -name '*.slnx')
        dotnet build $sln
    end
else if test "$command" = test
    for sln in (find -name '*.slnx')
        dotnet test $sln
    end
else
    echo "unknown command '$command'"
    return 1
end
