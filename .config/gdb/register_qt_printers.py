#!/bin/python

import sys
import os
import gdb
import re

gdb_path = os.path.expandvars("$XDG_CONFIG_HOME/gdb")
if gdb_path not in sys.path:
    sys.path.insert(0, gdb_path)

from helper import FunctionLookup
from qt_printers import *

pretty_printers_dict = {
    re.compile('^QString$'): lambda val: QStringPrinter(val),
    re.compile('^QByteArray$'): lambda val: QByteArrayPrinter(val),
    re.compile('^QList<.*>$'): lambda val: QListPrinter(val, 'QList', None),
    re.compile('^QStringList$'): lambda val: QListPrinter(val, 'QStringList', 'QString'),
    re.compile('^QQueue'): lambda val: QListPrinter(val, 'QQueue', None),
    re.compile('^QVector<.*>$'): lambda val: QVectorPrinter(val, 'QVector'),
    re.compile('^QStack<.*>$'): lambda val: QVectorPrinter(val, 'QStack'),
    re.compile('^QLinkedList<.*>$'): lambda val: QLinkedListPrinter(val),
    re.compile('^QMap<.*>$'): lambda val: QMapPrinter(val, 'QMap'),
    re.compile('^QMultiMap<.*>$'): lambda val: QMapPrinter(val, 'QMultiMap'),
    re.compile('^QHash<.*>$'): lambda val: QHashPrinter(val, 'QHash'),
    re.compile('^QMultiHash<.*>$'): lambda val: QHashPrinter(val, 'QMultiHash'),
    re.compile('^QDate$'): lambda val: QDatePrinter(val),
    re.compile('^QTime$'): lambda val: QTimePrinter(val),
    re.compile('^QDateTime$'): lambda val: QDateTimePrinter(val),
    re.compile('^QUrl$'): lambda val: QUrlPrinter(val),
    re.compile('^QSet<.*>$'): lambda val: QSetPrinter(val),
    re.compile('^QChar$'): lambda val: QCharPrinter(val),
    re.compile('^QUuid'): lambda val: QUuidPrinter(val),
    re.compile('^QVariant'): lambda val: QVariantPrinter(val),
}

gdb.pretty_printers.append(FunctionLookup(gdb, pretty_printers_dict))
