#!/usr/bin/env python
# coding: utf-8

from datetime import datetime, date, timedelta
from sys import argv

import requests

BASE_URL = 'http://api.twitter.com/1/'
DATE_RANGE = date.today() - timedelta(days=30)


def convert_date(date_string):
    return datetime.strptime(date_string, '%a %b %d %H:%M:%S +0000 %Y').date()


def request(url):
    r = requests.get(BASE_URL + url)
    if r.status_code == requests.codes.ok:
        return r.json()
    r.raise_for_status()


def get_following_ids(screen_name):
    r = request('friends/ids.json?screen_name={0:s}'.format(screen_name))
    return r['ids']


def get_user_tweets(user_id):
    r = request('users/show.json?user_id={0:d}'.format(user_id))
    if 'status' in r:
        result = (r['screen_name'],
                  convert_date(r['status']['created_at']))
    else:
        result = (r['screen_name'], None)
    return result


def main(screen_name, _reversed=False):
    following_ids = get_following_ids(screen_name)
    if _reversed:
        following_ids = reversed(following_ids)

    for user_id in following_ids:
        screen_name, last_tweet_date = get_user_tweets(user_id)
        print 'Checking {0:s}...'.format(screen_name)
        if isinstance(last_tweet_date, date):
            if last_tweet_date <= DATE_RANGE:
                print screen_name, last_tweet_date
        else:
            print '{0:s}\'s account is protected.'.format(screen_name)


def usage():
    msg = 'Usage: python {0} <TWITTER_SCREEN_NAME> [--reversed]'.format
    exit(msg(__file__))

if __name__ == '__main__':
    if len(argv) > 3 or not isinstance(argv[1], basestring):
        usage()
    _reversed = False
    if len(argv) == 3 and argv[2] == '--reversed':
        _reversed = True
    main(argv[1], _reversed)
