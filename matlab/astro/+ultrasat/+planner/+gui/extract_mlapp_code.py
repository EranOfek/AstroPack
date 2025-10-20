#!/usr/bin/env python3
"""
Extract MATLAB source code from all .mlapp files in the current directory
and save to ./mlapp_source/<name>_code.m

Works on both Windows and Linux.
Each .mlapp is a ZIP archive; code is stored as CDATA inside matlab/document.xml.

Author: Chen Tishler, 20/10/2025
"""

import zipfile
import os
import sys
import re


def extract_code_from_xml(xml_bytes):
    """Extract the CDATA block containing the MATLAB source code."""
    try:
        text = xml_bytes.decode("utf-8", errors="ignore")
        # Match <![CDATA[ ... ]]> that wraps the MATLAB code
        m = re.search(r"<!\[CDATA\[(.*?)\]\]>", text, re.DOTALL)
        if not m:
            return None
        return m.group(1).strip()
    except Exception as e:
        print(f"  ?? Failed to parse XML: {e}")
        return None


def extract_mlapp_code(src_folder='.'):
    out_folder = os.path.join(src_folder, 'mlapp_source')
    os.makedirs(out_folder, exist_ok=True)

    count = 0
    for file in os.listdir(src_folder):
        if file.lower().endswith('.mlapp'):
            mlapp_path = os.path.join(src_folder, file)
            name, _ = os.path.splitext(file)
            out_file = os.path.join(out_folder, f"{name}_code.m")

            try:
                with zipfile.ZipFile(mlapp_path, 'r') as z:
                    # Look for matlab/document.xml (modern MATLAB format)
                    candidates = [f for f in z.namelist() if f.endswith("matlab/document.xml")]
                    if not candidates:
                        print(f"Skipped (no matlab/document.xml): {file}")
                        continue

                    with z.open(candidates[0]) as f:
                        xml_data = f.read()

                    code = extract_code_from_xml(xml_data)
                    if code:
                        with open(out_file, "w", encoding="utf-8") as out:
                            out.write(code)
                        print(f"Extracted: {file} ? {out_file}")
                        count += 1
                    else:
                        print(f"No CDATA code found in {file}")

            except Exception as e:
                print(f"Failed to extract {file}: {e}")

    print(f"\nDone. Extracted {count} source file(s) into {out_folder}")


if __name__ == "__main__":
    folder = sys.argv[1] if len(sys.argv) > 1 else '.'
    extract_mlapp_code(folder)
