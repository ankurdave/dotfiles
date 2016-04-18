#!/usr/bin/env python

import imaplib2
import logging
import netrc
import socket
import subprocess
import sys
import time

logging.basicConfig(
    stream=sys.stdout,
    level=logging.DEBUG,
    format='%(asctime)s - %(message)s')
timeout = 5*60

def imap_password():
    return netrc.netrc().authenticators('imap.gmail.com')[2]

def ensure_connection_open(m):
    if m and m.state != 'LOGOUT':
        return m, False
    logging.info('Opening connection')
    m = imaplib2.IMAP4_SSL('imap.gmail.com', timeout=timeout)
    if m:
        return m, True
    else:
        raise AttributeError('IMAP4_SSL() returned %s' % (m,))

def ensure_logged_in(m):
    if m.state != 'NONAUTH':
        return
    logging.info('Logging in')
    logging.debug(m.login('ankurdave@gmail.com', imap_password()))

def ensure_mailbox_selected(m):
    if m.state == 'SELECTED':
        return
    logging.info('Selecting mailbox')
    logging.debug(m.examine('[Gmail]/All Mail'))

def cleanup_connection(m):
    try:
        if m is not None:
            logging.info('Cleaning up')
            logging.debug(m.close())
            logging.debug(m.logout())
    except Exception as e:
        logging.exception('Error while cleaning up: %s' % (e,))
    finally:
        return None

def main():
    m = None
    while True:
        try:
            m, just_opened = ensure_connection_open(m)
            ensure_logged_in(m)
            ensure_mailbox_selected(m)
            if just_opened:
                logging.info('Initial sync')
            else:
                logging.info('Waiting for new mail')
                logging.debug(m.idle(timeout))
            subprocess.Popen('/Users/ankurdave/repos/dotfiles/bin/sync-mail-once').wait()
        except KeyboardInterrupt:
            m = cleanup_connection(m)
            raise
        except Exception as e:
            logging.exception('Error: %s' % (e,))
            m = cleanup_connection(m)
            logging.info('Retrying in 60 seconds')
            time.sleep(60)

if __name__ == '__main__':
    main()
