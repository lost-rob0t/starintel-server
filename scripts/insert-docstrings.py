#!/usr/bin/env python3
"""Insert docstrings into Common Lisp definition forms.

Reads a doc table (JSON: {"PACKAGE::SYMBOL": "docstring", ...}) and patches
source files in place, adding docstrings to:
  defun / defmacro / defgeneric / defmethod  -> string after lambda list
  defvar / defparameter / defconstant        -> string after init form
  defstruct                                  -> string after name/options
  defclass / define-condition                -> (:documentation ...) option
Skips forms that already carry a docstring.  Symbol matching is
case-insensitive; entries may be given bare ("SYMBOL") or package
qualified ("PKG::SYMBOL"), in which case the file must define that package.
"""
import json
import re
import sys
import glob


def strip_comments_keep_spans(s):
    """Replace comment bodies with spaces (keeps offsets)."""
    out = list(s)
    i = 0
    n = len(s)
    while i < n:
        c = s[i]
        if c == '"':
            j = i + 1
            while j < n:
                if s[j] == '\\':
                    j += 2
                    continue
                if s[j] == '"':
                    break
                j += 1
            i = j + 1
            continue
        if c == ';':
            while i < n and s[i] != '\n':
                out[i] = ' '
                i += 1
            continue
        if c == '#' and i + 1 < n and s[i + 1] == '|':
            depth = 1
            j = i + 2
            while j < n and depth:
                if s[j] == '\\' and j + 1 < n:
                    j += 2
                    continue
                if s[j] == '#' and j + 1 < n and s[j + 1] == '|':
                    depth += 1
                    j += 2
                    continue
                if s[j] == '|' and j + 1 < n and s[j + 1] == '#':
                    depth -= 1
                    j += 2
                    continue
                j += 1
            for k in range(i, j):
                out[k] = ' '
            i = j
            continue
        i += 1
    return ''.join(out)


def balanced(src, start):
    """src[start] == '('; return index just past the matching ')'."""
    depth = 0
    i = start
    n = len(src)
    instr = False
    while i < n:
        c = src[i]
        if instr:
            if c == '\\':
                i += 2
                continue
            if c == '"':
                instr = False
            i += 1
            continue
        if c == '"':
            instr = True
            i += 1
            continue
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return i + 1
        i += 1
    return -1


def next_form_end(src, i):
    """Return index just past the form/atom starting at offset i."""
    n = len(src)
    while i < n and src[i].isspace():
        i += 1
    if i >= n:
        return i
    # quote family: read the quoted form after the marker
    if src[i] == "'" or src[i] == '`':
        return next_form_end(src, i + 1)
    if src[i] == '#' and i + 1 < n and src[i + 1] == "'":
        return next_form_end(src, i + 2)
    if src[i] == ',' and i + 1 < n and src[i + 1] == '@':
        return next_form_end(src, i + 2)
    if src[i] == ',':
        return next_form_end(src, i + 1)
    if src[i] == '(':
        return balanced(src, i)
    if src[i] == '"':
        j = i + 1
        while j < n:
            if src[j] == '\\':
                j += 2
                continue
            if src[j] == '"':
                return j + 1
            j += 1
        return n
    j = i
    while j < n and not src[j].isspace() and src[j] not in '()"\'':
        j += 1
    return j


def read_name(src, i):
    """Read the symbol starting at i; return (lowercase-name, end)."""
    n = len(src)
    while i < n and src[i].isspace():
        i += 1
    j = i
    while j < n and not src[j].isspace() and src[j] not in '()"\'':
        j += 1
    return src[i:j].lower(), j


def lisp_quote(text):
    """Escape a string for a Common Lisp literal."""
    return '"' + text.replace('\\', '\\\\').replace('"', '\\"') + '"'


def form_has_docstring(src, form_start, form_end, after_form_start):
    """True if the form already starts its body with a string literal."""
    i = after_form_start
    n = form_end
    while i < n and src[i].isspace():
        i += 1
    return i < n and src[i] == '"'


def find_and_patch(src, defkw):
    """Yield (def_start, def_end, insert_offset, kind) for each definition."""
    results = []
    for m in re.finditer(r'\(%s\s+' % defkw, src):
        start = m.start()
        end = balanced(src, start)
        if end < 0:
            continue
        results.append((start, end, m.end()))
    return results


def patch_file(path, wanted, record_accessors):
    """Apply docstring insertions to one file.

    WANTED maps lowercase symbol -> docstring. Returns (applied, skipped).
    """
    src = open(path).read()
    stripped = strip_comments_keep_spans(src)
    applied = []
    skipped = []

    # Collect patches as (insert_at, text, symbol) then apply from the end.
    patches = []

    def add_patch(pos, text, sym):
        patches.append((pos, text, sym))

    # defun / defmacro / defgeneric
    for kw in ('defun', 'defmacro', 'defgeneric'):
        for start, end, name_i in find_and_patch(stripped, kw):
            name, name_end = read_name(src, name_i)
            if name not in wanted:
                continue
            ll_end = next_form_end(stripped, name_end)
            if kw == 'defgeneric':
                # idempotent: any :documentation option means we are done
                if ':documentation' in stripped[start:end]:
                    skipped.append(name)
                    continue
                add_patch(ll_end,
                          '\n  (:documentation '
                          + lisp_quote(wanted[name]) + ')',
                          name)
            else:
                if form_has_docstring(src, start, end, ll_end):
                    skipped.append(name)
                    continue
                add_patch(ll_end, '\n  ' + lisp_quote(wanted[name]), name)

    # defvar / defparameter / defconstant
    for kw in ('defvar', 'defparameter', 'defconstant'):
        for start, end, name_i in find_and_patch(stripped, kw):
            name, name_end = read_name(src, name_i)
            if name not in wanted:
                continue
            val_end = next_form_end(stripped, name_end)
            if form_has_docstring(src, start, end, val_end):
                skipped.append(name)
                continue
            doc = wanted[name]
            add_patch(val_end, '\n  ' + lisp_quote(doc), name)

    # defstruct: docstring after the name/options form
    for start, end, name_i in find_and_patch(stripped, 'defstruct'):
        # name may be a bare symbol or a (name option...) list
        if stripped[name_i:name_i + 1] == '(':
            inner_end = balanced(stripped, name_i)
            name, _ = read_name(src, name_i + 1)
            opts_end = inner_end
        else:
            name, name_end = read_name(src, name_i)
            opts_end = name_end
        slot_docs = {k: v for k, v in wanted.items()
                     if k.startswith(name + '-')}
        for acc, slot in record_accessors.get(name, {}).items():
            if acc in wanted and acc not in slot_docs:
                slot_docs[acc] = wanted[acc]
        already = (';; Accessor documentation for %s\n' % name) in src
        if form_has_docstring(src, start, end, opts_end):
            if name in wanted:
                skipped.append(name)
            if slot_docs and not already:
                emit_accessor_block(end, name, slot_docs, add_patch)
            continue
        doc = wanted.get(name)
        if doc:
            add_patch(opts_end, '\n  ' + lisp_quote(doc), name)
        if slot_docs and not already:
            emit_accessor_block(end, name, slot_docs, add_patch)

    # defclass / define-condition: (:documentation "...") option before ')'
    for kw in ('defclass', 'define-condition'):
        for start, end, name_i in find_and_patch(stripped, kw):
            name, name_end = read_name(src, name_i)
            slot_docs = {k: v for k, v in wanted.items()
                         if k.startswith(name + '-')}
            for acc, slot in record_accessors.get(name, {}).items():
                if acc in wanted and acc not in slot_docs:
                    slot_docs[acc] = wanted[acc]
            already = (';; Accessor documentation for %s\n' % name) in src
            if name not in wanted and not slot_docs:
                continue
            if (name in wanted
                    and ':documentation' not in stripped[start:end]
                    and not already):
                doc = wanted[name]
                add_patch(end - 1,
                          '\n  (:documentation %s)' % lisp_quote(doc), name)
            # map accessor name -> slot name by walking slot specs
            acc_slot = {}
            if end > start:
                form = stripped[start:end]
                for sm in re.finditer(
                        r'\(\s*([a-zA-Z0-9*+-.]+)\s+([^()]*?'
                        r'(?:\([^()]*\)[^()]*?)*?)\)', form):
                    slot_name = sm.group(1).lower()
                    spec = sm.group(0)
                    for am in re.finditer(
                            r'(?::accessor|:reader)\s+([a-zA-Z0-9-]+)', spec):
                        acc_slot[am.group(1).lower()] = slot_name
            if slot_docs and not already:
                # prefer accurate slot names when we parsed them
                fixed_docs = {}
                for acc, sdoc in slot_docs.items():
                    slot_name = acc_slot.get(acc)
                    if slot_name and slot_name != acc:
                        fixed_docs[acc] = (
                            'The =%s= slot of =%s=.' % (slot_name, name))
                    else:
                        fixed_docs[acc] = sdoc
                emit_accessor_block(end, name, fixed_docs, add_patch)

    if not patches:
        return applied, skipped

    # Apply patches from the end so offsets stay valid.
    patches.sort(key=lambda p: p[0], reverse=True)
    out = src
    for pos, text, sym in patches:
        out = out[:pos] + text + out[pos:]
        applied.append(sym)

    open(path, 'w').write(out)
    return applied, skipped


def emit_accessor_block(end_pos, name, slot_docs, add_patch):
    lines = []
    for acc, sdoc in sorted(slot_docs.items()):
        lines.append('(setf (documentation \'%s \'function)\n%s)'
                     % (acc.upper(), lisp_quote(sdoc)))
    block = ('\n\n;; Accessor documentation for %s\n%s'
             % (name, '\n'.join(lines)))
    add_patch(end_pos, block, name)


def main():
    table_path, *paths = sys.argv[1:]
    raw = json.load(open(table_path))
    record_accessors = raw.get('_record_accessors', {})
    # normalize keys to lowercase bare symbols
    wanted = {k.lower().split('::')[-1]: v for k, v in raw.items()
              if not k.startswith('_')}
    wanted['_record_accessors'] = record_accessors
    total_applied, total_skipped = [], []
    files = []
    for p in paths:
        files.extend(sorted(glob.glob(p, recursive=True)))
    for path in files:
        a, s = patch_file(path, wanted, record_accessors)
        total_applied.extend(a)
        total_skipped.extend(s)
    print(json.dumps({
        'applied': sorted(set(total_applied)),
        'skipped_existing_doc': sorted(set(total_skipped)),
        'applied_count': len(set(total_applied)),
    }, indent=1))
    # report wanted symbols that were never found
    found = set(total_applied) | set(total_skipped)
    missing = sorted(set(wanted) - found)
    if missing:
        print(json.dumps({'not_found': missing}, indent=1))


if __name__ == '__main__':
    main()
