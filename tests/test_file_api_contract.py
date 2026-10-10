"""Supplemental API registration proof; native behavior lives in Lisp tests."""
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[1]


class FileApiRegistrationTests(unittest.TestCase):
    def test_defined_http_and_broker_file_operations_are_registered(self):
        contract = (ROOT / 'cli/http-contract-documents.lisp').read_text()
        self.assertIn(':id "files.create"', contract)
        self.assertIn(':id "files.content.get"', contract)
        system = (ROOT / 'source/starintel-gserver.asd').read_text()
        self.assertIn('(:file "databases/file-content")', system)
        self.assertIn('(:file "frontends/http-files")', system)
        rabbit = (ROOT / 'source/rabbit.lisp').read_text()
        self.assertIn(':handler-fn #\'handle-file-ingest', rabbit)
        self.assertIn('"files.ingest.#"', rabbit)


if __name__ == '__main__':
    unittest.main()
