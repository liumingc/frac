#!/usr/bin/env python3
import bs4
import requests
import sys,re,urllib,os,socket
from urllib.parse import urljoin
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

errcnt = 0
def handle_page_pics(page_url):
    page = get_bs4(page_url)

    #pics = page.find_all('a', attrs={'class':'col-thumbnail'})
    pics = page.find_all('a', attrs={'class':'ellipsis'})
    hrefs = []
    for pic in pics:
        if not pic.has_attr('href'):
            #print('ignore ...')
            continue
        href = urljoin(page_url, pic['href'])
        hrefs.append(href)

    for hr in hrefs:
        try:
            print('--> ' + hr)
            get_one_pic(hr)
        except Exception as e:
            with open('exc.log', 'a+') as f:
                global errcnt
                errcnt = errcnt + 1
                print('@error downloading for ' + hr + repr(e))
                print('@error there are %d error' % errcnt)
                f.write(hr + '\n')

    # to see if there is a next page
    next = page.find_all('a', attrs={'rel':'next'})
    for x in next:
        nexturl = urljoin(page_url, x['href'])
        handle_page_pics(nexturl)


def get_one_pic(hr):
    page = get_bs4(hr)
    img = page.find('div', attrs={'id':'theImage'})
    if not img:
        print('no theImage')
        return
    src = urljoin(hr, img.img['src'])
    folder = re.search('.*/(.*)', hr).group(1)
    os.system('mkdir -p ' + folder)
    dst = folder + '/' + re.search('.*/(.*)', src).group(1)
    if os.path.exists(dst):
        print(dst + ' already exist')
        return
    header = {
            'Referer': hr
            }
    rsp = requests.get(src, headers=header, timeout=120) # 120sec should be enough

    with open(dst, 'wb') as f:
        f.write(rsp.content)
        f.flush()
    print(src + ' done.')



if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: crawl <filename>')
        sys.exit(0)

    socket.setdefaulttimeout(240)
    f = open(sys.argv[1])
    for url in f:
        handle_page_pics(url[:-1])
    f.close()
