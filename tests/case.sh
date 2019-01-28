f () {
    case "$1" in
	upgrade|update)
	    echo foo
	    ;;

	(clean)
	    echo bar
	    ;;

	*)
	    echo baz
    esac
}

f upgrade
f "clean"
f cleana
f shproutz
