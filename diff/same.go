package diff

import (
	"github.com/tomingtoming/slicediff"
	"github.com/tomingtoming/zipdiff/zip"
)

func IsSame(s, d slicediff.Comparerable) bool {
	if src, ok := s.(zip.ZipElem); !ok {
		panic("???")
	} else if dst, ok := d.(zip.ZipElem); !ok {
		panic("???")
	} else {
		return src.HashValue == dst.HashValue && src.Size == dst.Size && src.Name == dst.Name
	}
}
