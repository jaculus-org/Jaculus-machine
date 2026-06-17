#!/usr/bin/env python3
"""Test262 runner - runs JS test files against a test harness executable."""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
import yaml


FRONT_MATTER_RE = re.compile(r'/\*---\s*\n(.*?)\n\s*---\*/', re.DOTALL)


def parse_front_matter(filepath: str) -> dict:
    """Extract and parse YAML front matter from a test262 JS file."""
    with open(filepath, 'r') as f:
        content = f.read()

    match = FRONT_MATTER_RE.search(content)
    if not match:
        return {}

    try:
        metadata = yaml.safe_load(match.group(1))
        return metadata if metadata else {}
    except yaml.YAMLError as e:
        print(f"Warning: Failed to parse YAML in {filepath}: {e}", file=sys.stderr)
        return {}


def run_single(executable: str, test_file: str, is_module: bool,
               harness_dir: str, timeout: int) -> dict:
    """Run a single test variant. Returns parsed JSON result dict."""
    cmd = [executable, '--path', test_file, '--format', 'json']
    if is_module:
        cmd.append('--module')
    if harness_dir:
        cmd.extend(['--harness-dir', harness_dir])

    try:
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout
        )
        stdout = proc.stdout.strip()
        stderr = proc.stderr.strip()

        # JSON result is the last line of stdout
        lines = stdout.splitlines()
        for line in reversed(lines):
            line = line.strip()
            if line.startswith('{') and line.endswith('}'):
                try:
                    return json.loads(line)
                except json.JSONDecodeError:
                    continue

        return {
            'result': 'fail',
            'phase': 'runtime',
            'error': f'No valid JSON output. stdout: {stdout[-500:]}, stderr: {stderr[-500:]}',
            'type': 'Error',
        }

    except subprocess.TimeoutExpired:
        return {
            'result': 'fail',
            'phase': 'runtime',
            'error': f'Test timed out after {timeout}s',
            'type': 'Error',
        }
    except Exception as e:
        return {
            'result': 'fail',
            'phase': 'runtime',
            'error': f'Runner error: {e}',
            'type': 'Error',
        }


def check_negative(result: dict, negative: dict) -> bool:
    """Check if a negative test result matches expectations.

    Returns True if it failed as expected.
    """
    expected_phase = negative.get('phase', '*')
    expected_type = negative.get('type', '*')

    if result.get('result') != 'fail':
        return False

    if expected_phase != '*' and result.get('phase', '') != expected_phase:
        return False

    if expected_type != '*' and result.get('type', '') != expected_type:
        return False

    return True


def write_includes(tmp_file, metadata: dict, harness_dir: str) -> None:
    """Write harness include files into a temp file if present."""
    for inc in metadata.get('includes', []):
        hp = os.path.join(harness_dir, inc)
        if os.path.isfile(hp):
            with open(hp, 'r') as hf:
                tmp_file.write(hf.read())
                tmp_file.write('\n')


def run_test(executable: str, test_path: str, metadata: dict,
             test_root: str, harness_dir: str) -> dict:
    """Run a test file with all necessary variants (strict/non-strict)."""
    full_path = os.path.join(test_root, test_path)
    flags = metadata.get('flags') or []
    negative = metadata.get('negative')
    is_module = 'module' in flags
    is_async = 'async' in flags
    only_strict = 'onlyStrict' in flags
    no_strict = 'noStrict' in flags or 'raw' in flags

    timeout = 30 if is_async else 10

    temp_files = []

    def run_variant(prepend_strict: bool, label: str) -> dict:
        if prepend_strict and is_module:
            return None

        if prepend_strict or 'includes' in metadata:
            tmp_dir = os.path.dirname(full_path)
            prefix = '_strict_' if prepend_strict else '_includes_'
            tmp_file = tempfile.NamedTemporaryFile(
                mode='w', suffix='.js', delete=False, dir=tmp_dir, prefix=prefix
            )
            if prepend_strict:
                tmp_file.write('"use strict";\n')
            write_includes(tmp_file, metadata, harness_dir)
            with open(full_path, 'r') as f:
                tmp_file.write(f.read())
            tmp_file.flush()
            tmp_file.close()
            temp_files.append(tmp_file.name)
            test_file = tmp_file.name
        else:
            test_file = full_path

        result = run_single(executable, test_file, is_module, harness_dir, timeout)
        result['_label'] = label
        return result

    if only_strict:
        variants = [v for v in [run_variant(True, 'strict')] if v is not None]
    elif no_strict:
        variants = [v for v in [run_variant(False, 'default')] if v is not None]
    else:
        variants = [v for v in [
            run_variant(False, 'default'),
            run_variant(True, 'strict'),
        ] if v is not None]

    for f in temp_files:
        try:
            os.unlink(f)
        except OSError:
            pass

    if negative:
        failures = [v for v in variants if not check_negative(v, negative)]
        if not failures:
            return {
                'test': test_path,
                'result': 'PASS',
                'message': f'Expected failure ({negative.get("phase")}/{negative.get("type")})',
                'variants': variants,
            }
        else:
            return {
                'test': test_path,
                'result': 'FAIL',
                'message': 'Negative test did not match expected failure',
                'variants': variants,
                'expected': negative,
            }
    else:
        failures = [v for v in variants if v.get('result') != 'pass']
        if not failures:
            return {
                'test': test_path,
                'result': 'PASS',
                'message': 'All variants passed',
                'variants': variants,
            }
        else:
            return {
                'test': test_path,
                'result': 'FAIL',
                'message': f'Test failed in variants: {[v["_label"] for v in variants if v.get("result") != "pass"]}',
                'variants': variants,
            }


def main():
    parser = argparse.ArgumentParser(
        description='Test262 runner for JS engine test harnesses'
    )
    parser.add_argument(
        '--executable', required=True,
        help='Path to the entry point executable (e.g., build/tests/run-quickjs)'
    )
    parser.add_argument(
        '--test-list', required=True,
        help='Path to YAML test list file'
    )
    parser.add_argument(
        '--filter', default=None,
        help='Filter results: "pass" or "fail"'
    )
    parser.add_argument(
        '--format', default='text',
        choices=['text', 'json'],
        help='Output format (default: text)'
    )

    args = parser.parse_args()

    with open(args.test_list, 'r') as f:
        config = yaml.safe_load(f)

    test_root = config.get('test_root', 'test')
    harness_dir = config.get('harness_dir', '')

    if not os.path.isdir(test_root):
        sys.exit(f"Error: test_root directory not found: {test_root}")

    executable = args.executable

    if not os.path.isfile(executable):
        sys.exit(f"Error: executable not found: {executable}")

    test_list = config.get('tests', [])
    if not test_list:
        sys.exit('Error: No tests found in test list')

    if args.filter:
        test_list = [t for t in test_list if args.filter in t]

    results = []
    passed = 0
    failed = 0

    start_time = time.time()

    for test_path in test_list:
        full_path = os.path.join(test_root, test_path)
        if not os.path.isfile(full_path):
            result = {
                'test': test_path,
                'result': 'FAIL',
                'message': f'File not found: {full_path}',
                'variants': [],
            }
            results.append(result)
            failed += 1
            continue

        metadata = parse_front_matter(full_path)
        result = run_test(executable, test_path, metadata, test_root, harness_dir)
        results.append(result)

        if result['result'] == 'PASS':
            passed += 1
        else:
            failed += 1

        if args.format == 'text':
            status = 'PASS' if result['result'] == 'PASS' else 'FAIL'
            print(f"[{status}] {test_path}")
            if result['result'] == 'FAIL':
                print(f"       {result['message']}")
                for v in result.get('variants', []):
                    if v.get('result') != 'pass':
                        print(f"       {v['_label']}: {v.get('error', '')}")

    elapsed = time.time() - start_time

    if args.format == 'json':
        output = {
            'results': results,
            'summary': {
                'total': len(results),
                'passed': passed,
                'failed': failed,
                'time': round(elapsed, 2),
            },
        }
        print(json.dumps(output, indent=2))
    else:
        print(f"\n{passed} passed, {failed} failed, {len(results)} total ({elapsed:.2f}s)")
        if failed > 0:
            sys.exit(1)


if __name__ == '__main__':
    main()
