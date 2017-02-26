#!/usr/bin/env python3
import bs4
import requests
import sys,re,urllib,os
import threading


def get_bs4(url):
    rsp = requests.get(url)
    txt = rsp.text

    encoding = ['gbk', 'gb2312']
    for enc in encoding:
        try:
            ntxt = txt.encode(rsp.encoding).decode('gbk')
            break
        except Exception as e:
            pass
    #print(ntxt)
    obj = bs4.BeautifulSoup(ntxt, 'lxml')
    return obj

def handle_one(main_url):
    bo = get_bs4(main_url)

    # get page_list
    pages = {}

    g = re.search('.*/(.*).html', main_url)
    if g:
        base = g.group(1)
    else:
        base = 'anon'

    pages[main_url] = True
    ul_list = bo.find_all('ul', attrs={'class':'pagelist'})
    #print(ul_list)
    for ul in ul_list:
        for li in ul.find_all('li'):
            #print(li)
            if li.has_attr('href'):
                page_url = urllib.parse.urljoin(main_url, li['href'])
                #print(page_url)
                pages[page_url] = True
            elif li.a.has_attr('href'):
                page_url = urllib.parse.urljoin(main_url, li.a['href'])
                print(page_url)
                pages[page_url] = True

    for page in pages:
        print('handleing page ' + page)
        download_pic_from_page(base, page)

def download_pic(src, base):
    print('downloading pic ' + src + ' ...')
    dst = base + '/' + re.search('.*/(.*)', src).group(1)
    urllib.request.urlretrieve(src, dst)
    print(src + ' done.')

def multi_download(srcs, base):
    n = 7
    while True:
        if len(srcs) == 0:
            break
        if len(srcs) > n:
            subsrcs = srcs[:n]
            srcs = srcs[n:]
        else:
            subsrcs = srcs
            srcs = []

        tasks = []
        for x in subsrcs:
            task = threading.Thread(target=download_pic, args=(x, base))
            tasks.append(task)
            task.start()

        for task in tasks:
            task.join(65)   # timeout=15 secs

#def multi_download(srcs, base):
#    for src in srcs:
#        download_pic(src, base)


def download_pic_from_page(base, page):
    os.system('mkdir -p ' + base)
    bo = get_bs4(page)
    img_list = bo.find_all('div', attrs={'class':'big_img'})
    srcs = []
    for img in img_list:
        for x in img.find_all('img'):
            if x.has_attr('src'):
                src = x['src']
                srcs.append(src)
                #picname = re.search('.*/(.*)', src).group(1)
                #urllib.request.urlretrieve(src, base + '/' + picname)

    multi_download(srcs, base)

if __name__ == '__main__':
    #for url in sys.argv[1:]:
    #    print(url)
    #    handle_one(url)
    f = open('url.txt')
    for url in f:
        handle_one(url[:-1])
    f.close()
