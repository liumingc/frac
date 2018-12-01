#!/usr/bin/env python3
import bs4
import requests
import sys,re,urllib,os,socket
from urllib.parse import urljoin
import threading
import time

def get_with_retry(url, **kwargs):
    def aux(cnt):
        if cnt < 3:
            try:
                rsp = requests.get(url, timeout=100, **kwargs)
            except:
                aux(cnt+1)
        return rsp
    return aux(0)

def get_bs4(url, cnt):
    rsp = get_with_retry(url)
    txt = rsp.text

    encoding = ['gbk', 'gb2312']
    for enc in encoding:
        try:
            ntxt = txt.encode(rsp.encoding).decode(enc)
            break
        except Exception as e:
            pass

    if ntxt.find('slowly and enjoy :)'):
        #try again
        if cnt > 0:
            time.sleep(5)
            return get_bs4(url, cnt-1)

    #print(ntxt)
    obj = bs4.BeautifulSoup(ntxt, 'lxml')
    return obj

errcnt = 0
def _handle_page_pics(page_url):
    page = get_bs4(page_url, 3)

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
    nxt = page.find_all('a', attrs={'rel':'next'})
    for x in nxt:
        nexturl = urljoin(page_url, x['href'])
        handle_page_pics(nexturl)

def handle_page_pics(page_url):
    try:
        _handle_page_pics(page_url)
    except:
        pass

def get_one_pic(hr):
    page = get_bs4(hr, 3)
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
    #rsp = requests.get(src, headers=header, timeout=50)
    rsp = get_with_retry(src, headers=header)

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
