package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"
)

var (
	destDir string
	pattern string
	jobSize int
)

func main() {
	flag.StringVar(&destDir, "d", ".", "save destination dir(default current dir)")
	flag.StringVar(&pattern, "p", "*.jpg", "download resource matching this pattern")
	flag.IntVar(&jobSize, "j", 4, "concurrent download job size")
	flag.Parse()

	if flag.NArg() == 0 {
		flag.Usage()
		return
	}

	_, err := os.Stat(destDir)
	if os.IsNotExist(err) {
		err = os.MkdirAll(destDir, 0755)
		if err != nil {
			fmt.Fprintf(os.Stderr, "mkdir %s failed: %v\n", destDir, err)
			return
		}
	} else if err != nil {
		fmt.Fprintf(os.Stderr, "Access destDir %s failed: %v\n", destDir, err)
		return
	}
	mainpageurl := flag.Arg(0)
	handle(mainpageurl)
}

func handle(baseUrl string) {
	content, err := downloadMainPage(baseUrl)
	if err != nil {
		return
	}
	downloadRes(baseUrl, content)
}

func downloadMainPage(baseUrl string) (string, error) {
	resp, err := http.Get(baseUrl)
	if err != nil {
		fmt.Fprintf(os.Stderr, "download main page failed: %v\n", err)
		return "", err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetch main page content failed: %v\n", err)
		return "", nil
	}
	return string(body), nil
}

func downloadRes(baseUrl string, content string) {
	//fmt.Println("content: =>\n", content)
	re := regexp.MustCompile(`(href|src|HREF|SRC)="[^"]+"`)
	reuser := regexp.MustCompile(pattern)
	links := re.FindAllString(content, -1)
	reslinks := []string{}
	for _, link := range links {
		base, _ := url.Parse(baseUrl)
		var sublink string
		if strings.ToLower(link[:3]) == "src" {
			sublink = link[5 : len(link)-1]
		} else {
			sublink = link[6 : len(link)-1]
		}
		if !reuser.MatchString(sublink) {
			continue
		}
		u, err := url.Parse(sublink)
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse link {%s} failed: %v\n", sublink, err)
			continue
		}
		reslink := base.ResolveReference(u).String()
		//fmt.Printf("link={%s}, reslink={%s}\n", sublink, reslink)
		fmt.Printf("link={%s}\n", sublink)
		reslinks = append(reslinks, reslink)
	}

	c := make(chan string, jobSize)
	var wg sync.WaitGroup

	wg.Add(jobSize)
	for i := 0; i < jobSize; i++ {
		go func(i int) {
			for res := range c {
				var status string
				start := time.Now()
				fmt.Printf("[%d] start downloading {%s}\n", i, res)
				if err := downloadOneRes(res); err != nil {
					status = "fail"
					fmt.Printf("[%d] download %s err: %v\n", i, res, err)
				} else {
					status = "succ"
				}
				t := time.Now()
				elapsed := t.Sub(start)
				fmt.Printf("[%d] dowload {%s} %s, elapsed %s, done.\n", i, res, status, elapsed)
			}
			wg.Done()
		}(int(i))
	}

	go func() {
		for _, link := range reslinks {
			c <- link
		}
		close(c)
	}()

	wg.Wait()
	fmt.Println("Done.")
}

func downloadOneRes(resUrl string) error {
	resp, err := http.Get(resUrl)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil
	}
	fileName := filepath.Join(destDir, extractFilename(resUrl))
	if len(fileName) == 0 {
		return fmt.Errorf("Extract filename from {%s} failed", resUrl)
	}
	return ioutil.WriteFile(fileName, body, 0644)
}

func extractFilename(resUrl string) string {
	if resUrl[len(resUrl)-1] == '/' {
		resUrl = resUrl[:len(resUrl)-1]
	}
	idx := strings.LastIndex(resUrl, "/")
	if idx == -1 {
		return ""
	}
	return resUrl[idx+1:]
}
