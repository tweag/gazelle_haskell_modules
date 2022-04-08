package gazelle_haskell_modules

import (
	"testing"
)

func TestGetSrcsFromCommentsEmptySlice(t *testing.T) {
	input := []string{}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail on an empty slice")
	}
	if len(srcs) != 0 {
		t.Fatalf("expected getSrcDirsFromComments to return an empty slice on an empty slice")
	}
}

func TestGetSrcsFromCommentsNoDirective(t *testing.T) {
	input := []string{"#asdf", "# foo", "#bar", "# pirin"}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail on a slice with no directive in any of the strings")
	}
	if len(srcs) != 0 {
		t.Fatalf("expected getSrcDirsFromComments to return an empty slice with no directive in any of the strings")
	}
}

func TestGetSrcsFromCommentsMoreThanOneDirective(t *testing.T) {
	input := []string{"#" + PRIVATE_FIND_MODULES_DIRECTIVE, "## foo", "#   " + PRIVATE_FIND_MODULES_DIRECTIVE + " bar", "# pirin"}
	_, err := getSrcDirsFromComments(input)
	if err == nil {
		t.Fatalf("expected getSrcDirsFromComments to fail on a slice with more than one directive in the strings")
	}
}

func TestGetSrcsFromCommentsOneDirectiveOneSrc(t *testing.T) {
	input := []string{"#" + PRIVATE_FIND_MODULES_DIRECTIVE + " src/", "## foo", "# pirin"}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail on a slice with one directive in the strings")
	}
	if len(srcs) != 1 {
		t.Fatalf("expected getSrcDirsFromComments to find one src folder")
	}
	if srcs[0] != "src/" {
		t.Fatalf("expected getSrcDirsFromComments to find \"src/\"")
	}
}

func TestGetSrcsFromCommentsOneDirectiveOneWithSpaces(t *testing.T) {
	input := []string{"#   \t  " + PRIVATE_FIND_MODULES_DIRECTIVE + " src/", "## foo", "# pirin"}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail on a slice with one directive with arbitrary whitespace before the directive")
	}
	if len(srcs) != 1 {
		t.Fatalf("expected getSrcDirsFromComments to find one src folder")
	}
	if srcs[0] != "src/" {
		t.Fatalf("expected getSrcDirsFromComments to find \"src/\"")
	}
}

func TestGetSrcsFromCommentsOneDirectiveMultipleSrc(t *testing.T) {
	input := []string{"#" + PRIVATE_FIND_MODULES_DIRECTIVE + " src/ pirin/ lol", "## foo", "# pirin"}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail on a slice with one directive in the strings")
	}
	if len(srcs) != 3 {
		t.Fatalf("expected getSrcDirsFromComments to find three src folders")
	}
	if srcs[0] != "src/" {
		t.Fatalf("expected getSrcDirsFromComments to find \"src/\"")
	}
	if srcs[1] != "pirin/" {
		t.Fatalf("expected getSrcDirsFromComments to find \"pirin/\"")
	}
	if srcs[2] != "lol" {
		t.Fatalf("expected getSrcDirsFromComments to find \"lol\"")
	}
}

func TestGetSrcsFromCommentsDirectiveNotLeading(t *testing.T) {
	input := []string{"# pesho " + PRIVATE_FIND_MODULES_DIRECTIVE + " src/", "## foo", "# pirin"}
	srcs, err := getSrcDirsFromComments(input)
	if err != nil {
		t.Fatalf("expected getSrcDirsFromComments to not fail when the directive isn't leading")
	}
	if len(srcs) != 0 {
		t.Fatalf("expected getSrcDirsFromComments to return no src folders when the directive isn't leading")
	}
}
