#!/usr/bin/env python3
# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4

import sys
import re
import urllib.request

def avg(item):
    """ Calculate the average"""

    # item is (conf, (poss, games))
    return item[1][0]/item[1][1]

def get_matches(str):
    """ Returns a list of tuples: (poss, team, conference, games) """
    m = re.findall('<td.*?>(\d+\.\d+)</td>.*?' \
                   '<a href=.*?teams.*?>(.*?)</a>.*?' \
                   '<a href=.*?conferences.*?>(.*?)</a>.*?' \
                   '<td>(\d+)</td>', 
                   str, re.MULTILINE | re.DOTALL)
    return m

def read_stat(filename):
    matches = get_matches(filename)

    conf = {}
    for t in matches:
        name = t[2]
        team = t[1]
        data = (int(float(t[0])), int(t[3])) # a tuple (poss, games)

        if name == 'Independents':
            name = 'I-' + team

        if not name in conf:
            conf[name] = data
        else:
            # Add each individual tuple field for data to conf[name
            conf[name] = tuple([sum(x) for x in zip(conf[name], data)])

    # sort based on the value returned by the average
    for i in sorted(conf.items(), key=avg, reverse=True):
        print('{0:25} {1:0.2f}'.format(i[0], avg(i)))


def main():
    args = sys.argv[1:]


    if args:
        f = open(args[0], 'r')
        read_stat(f.read())
        f.close()
    else:
        opener = urllib.request.build_opener()
        opener.addheaders = [('User-agent', 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/40.0.2214.115 Safari/537.36')]
        o = opener.open('http://statsheet.com/mcb/teams/stats?season=2014-2015&conf=&games=&stat=possessions&stat_type=');
        read_stat(o.read().decode('utf-8'))


if __name__ == '__main__':
    main()
