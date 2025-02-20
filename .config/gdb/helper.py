import sys

PY2 = sys.version_info[0] == 2
PY3 = sys.version_info[0] == 3

if PY3:
    Iterator = object
else:
    class Iterator(object):

        def next(self):
            return type(self).__next__(self)
if PY3:
    unichr = chr
else:
    unichr = unichr

def has_field(val, name):
    """Check whether @p val (gdb.Value) has a field named @p name"""
    try:
        val[name]
        return True
    except Exception:
        return False

def default_iterator(val):
    for field in val.type.fields():
        yield field.name, val[field.name]

class FunctionLookup:

    def __init__(self, gdb, pretty_printers_dict):
        self.gdb = gdb
        self.pretty_printers_dict = pretty_printers_dict

    def __call__(self, val):
        "Look-up and return a pretty-printer that can print val."

        # Get the type.
        type = val.type;

        # If it points to a reference, get the reference.
        if type.code == self.gdb.TYPE_CODE_REF:
            type = type.target ()

        # Get the unqualified type, stripped of typedefs.
        type = type.unqualified ().strip_typedefs ()

        # Get the type name.
        typename = type.tag
        if typename == None:
            return None

        # Iterate over local dictionary of types to determine
        # if a printer is registered for that type.  Return an
        # instantiation of the printer if found.
        for function in self.pretty_printers_dict:
            if function.search (typename):
                return self.pretty_printers_dict[function](val)

        # Cannot find a pretty printer.  Return None.
        return None
