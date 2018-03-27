package zip

import (
	"archive/zip"
	"os"
	"log"
	"io"
	"time"
	"fmt"
	"bytes"
	"io/ioutil"
	"crypto/sha256"
	"strings"
)

func GetInfo(path string) ([]ZipElem, error) {
	file, err := os.Open(path)
	defer file.Close()
	if err != nil {
		log.Fatalf("File open error: %s", path)
		return nil, err
	} else if fi, err := file.Stat(); err != nil {
		log.Fatalf("File open error: %s", path)
		return nil, err
	} else {
		return getZipInfoSlice(file, fi.Size(), "")
	}
}

func getZipInfoSlice(r io.ReaderAt, size int64, prefix string) ([]ZipElem, error) {
	zipInfoSlice := []ZipElem{}
	if zipReader, err := zip.NewReader(r, size); err != nil {
		return nil, err
	} else {
		for _, file := range zipReader.File {
			newZipElemName := /* prefix + */ file.Name
			if r, err := file.Open(); err != nil {
				return nil, err
			} else if buf, err := ioutil.ReadAll(r); err != nil {
				return nil, err
			} else {
				if strings.HasSuffix(strings.ToLower(file.Name), ".zip") {
					if zipElems, err := getZipInfoSlice(bytes.NewReader(buf), int64(file.UncompressedSize64), newZipElemName + "/"); err == nil {
						zipInfoSlice = append(zipInfoSlice, zipElems...)
					}
				}
				newZipElem := ZipElem{
					ModTime:   file.ModTime(),
					HashValue: fmt.Sprintf("%064x", sha256.Sum256(buf)),
					Size:      int64(file.UncompressedSize64),
					Name:      newZipElemName,
				}
				zipInfoSlice = append(zipInfoSlice, newZipElem)
			}
		}
	}
	return zipInfoSlice, nil
}

type ZipElem struct {
	ModTime   time.Time
	HashValue string
	Size      int64
	Name      string
}

func (t ZipElem) Key() string {
	return t.Name
}

func (t ZipElem) String() string {
	str := fmt.Sprintf("%s %s %4s %s", t.ModTime, t.HashValue[:7], t.showSize(), t.Name)
	return str
}

func (t ZipElem) showSize() string {
	n := t.Size
	switch {
	case n < 1000:
		return fmt.Sprintf("%d", n)
	case n < 10000:
		return fmt.Sprintf("%d", n / 1000) + "K"
	case n < 1000000:
		return fmt.Sprintf("%d", n / 1000.0) + "K"
	case n < 10000000:
		return fmt.Sprintf("%d", n / 1000000) + "M"
	case n < 1000000000:
		return fmt.Sprintf("%d", n / 1000000.0) + "M"
	case n < 10000000000:
		return fmt.Sprintf("%d", n / 1000000000) + "G"
	case n < 1000000000000:
		return fmt.Sprintf("%d", n / 1000000000.0) + "G"
	case n < 10000000000000:
		return fmt.Sprintf("%d", n / 1000000000000) + "T"
	case n < 1000000000000000:
		return fmt.Sprintf("%d", n / 1000000000000.0) + "T"
	default:
		return fmt.Sprintf("%d", n)
	}
}
