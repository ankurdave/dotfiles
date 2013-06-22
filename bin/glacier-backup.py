#!/usr/bin/env python

import os
import subprocess
import sys

if len(sys.argv) < 2:
  print """Usage: glacier-backup.py SRC_DIR DST_VAULT"""
  sys.exit(1)

src_dir = sys.argv[1]
dst_vault = sys.argv[2]

def dirSize(directory):
  """Return the size of the given directory in kilobytes."""
  du = subprocess.check_output(['du', '-sk', directory])
  return int(du.split()[0])

def backupSingleDir(directory):
  tar = subprocess.Popen(['tar', '-c', directory], stdout=subprocess.PIPE)
  pv = subprocess.Popen(['pv'], stdin=tar.stdout, stdout=subprocess.PIPE)
  glacier = subprocess.Popen(['glacier', 'archive', 'upload', '--name',
                              directory + '.tar', dst_vault, '-'],
                             stdin=pv.stdout)

def backupSingleFile(f):
  pv = subprocess.Popen(['pv', f], stdout=subprocess.PIPE)
  glacier = subprocess.Popen(['glacier', 'archive', 'upload', '--name',
                              f, dst_vault, '-'],
                             stdin=pv.stdout)

def alreadyPresent(f):
  glacier = subprocess.check_output(['glacier', 'archive', 'checkpresent',
                                     dst_vault, f], stderr=subprocess.PIPE)
  return glacier

def backup(directory):
  for f_basename in os.listdir(directory):
    f = os.path.join(directory, f_basename)
    if alreadyPresent(f) or alreadyPresent(f + '.tar'):
      print "%s already present; skipping" % f
    else:
      if os.path.isdir(f):
        dir_size = dirSize(f)
        if dir_size > 45:
          print "%s too big (%d kB > 45 kB); recursing" % (f, dir_size)
          backup(f)
        else:
          print "Backing up dir %s (%d kB)" % (f, dir_size)
          backupSingleDir(f)
      else:
        print "Backing up file %s" % f
        backupSingleFile(f)

backup(src_dir)
