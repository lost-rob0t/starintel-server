"""Execute the actual CouchDB FTS index against reserved execution records."""
import json
from pathlib import Path
import subprocess
import unittest


class JournalIndexPrivacy(unittest.TestCase):
    def test_private_rows_never_enter_search_index(self):
        root = Path(__file__).resolve().parents[1]
        index = json.loads((root / "source/views/search.json").read_text())["indexes"]["fts"]["index"]
        fixtures = [
            {"_id": "router-execution:" + "a" * 64, "content": "private"},
            {"id": "router-execution:" + "b" * 64, "dtype": "note", "content": "private"},
            {"_id": "ordinary", "type": "_server_router_execution", "content": "private"},
        ]
        script = "const fn=" + index + ";let rows=[];global.index=(...x)=>rows.push(x);\n"
        script += "for(const doc of " + json.dumps(fixtures) + ") {rows=[];fn(doc);if(rows.length)throw Error('private record indexed');}\n"
        script += "rows=[];fn({_id:'normal',dtype:'note',dataset:'fixture',content:'synthetic'});if(!rows.some(x=>x[0]==='content'))throw Error('ordinary record absent');"
        subprocess.run(["node", "-e", script], check=True, timeout=10)


if __name__ == "__main__":
    unittest.main()
