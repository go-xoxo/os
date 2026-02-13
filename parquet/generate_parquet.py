#!/usr/bin/env python3
"""
Generate parquet database files from repository file investigation.
Investigates ALL file extensions across the entire repo tree,
including dist-newstyle build artifacts & dependency sources.
"""

import pyarrow as pa
import pyarrow.parquet as pq
import os
import hashlib
from datetime import datetime

REPO_ROOT = "/home/user/os"
PARQUET_DIR = os.path.join(REPO_ROOT, "parquet")

# Only exclude .git and our own output directory
EXCLUDE_DIRS = {".git", "parquet"}


def get_file_hash(filepath):
    try:
        h = hashlib.sha256()
        with open(filepath, "rb") as f:
            for chunk in iter(lambda: f.read(8192), b""):
                h.update(chunk)
        return h.hexdigest()
    except Exception:
        return ""


def count_lines(filepath):
    try:
        with open(filepath, "r", errors="replace") as f:
            return sum(1 for _ in f)
    except Exception:
        return 0


def is_binary(filepath):
    try:
        with open(filepath, "rb") as f:
            chunk = f.read(1024)
            return b"\x00" in chunk
    except Exception:
        return False


def get_extension(fname):
    """Get file extension, handling compound extensions like .hs-boot."""
    if fname.endswith(".hs-boot"):
        return ".hs-boot"
    if fname.endswith(".hi-boot"):
        return ".hi-boot"
    if fname.endswith(".o-boot"):
        return ".o-boot"
    _, ext = os.path.splitext(fname)
    return ext


def get_category(ext):
    categories = {
        ".hs": "Haskell Source",
        ".hs-boot": "Haskell Boot",
        ".hi": "Haskell Interface",
        ".hi-boot": "Haskell Interface Boot",
        ".o": "Object File",
        ".o-boot": "Object Boot File",
        ".cabal": "Cabal Config",
        ".yaml": "YAML Config",
        ".yml": "YAML Config",
        ".json": "JSON Config",
        ".md": "Markdown Documentation",
        ".lock": "Lock File",
        ".h": "C Header",
        ".sample": "Sample/Template",
        ".exe": "Executable Binary",
        ".cache": "Cache File",
        ".rev": "Revision File",
        ".pack": "Pack File",
        ".idx": "Index File",
        "": "No Extension",
    }
    return categories.get(ext, "Other")


def classify_source(relpath):
    """Classify file as project source, dependency, or build artifact."""
    if relpath.startswith("dist-newstyle/tmp/"):
        return "dependency_source"
    elif relpath.startswith("dist-newstyle/build/"):
        return "build_artifact"
    elif relpath.startswith("dist-newstyle/cache/"):
        return "build_cache"
    elif relpath.startswith("dist-newstyle/packagedb/"):
        return "package_db"
    elif relpath.startswith("dist-newstyle/"):
        return "build_other"
    elif relpath.startswith(".github/"):
        return "ci_config"
    elif relpath.startswith(".vscode/"):
        return "editor_config"
    elif relpath.startswith("notes/"):
        return "documentation"
    else:
        return "project_source"


def collect_all_files():
    """Walk entire repository and collect metadata for every file."""
    files = []
    for root, dirs, filenames in os.walk(REPO_ROOT):
        dirs[:] = [d for d in dirs if d not in EXCLUDE_DIRS]
        for fname in filenames:
            filepath = os.path.join(root, fname)
            relpath = os.path.relpath(filepath, REPO_ROOT)
            try:
                stat = os.stat(filepath)
            except Exception:
                continue
            ext = get_extension(fname)
            binary = is_binary(filepath)
            files.append({
                "file_path": relpath,
                "file_name": fname,
                "extension": ext if ext else "(none)",
                "category": get_category(ext),
                "source_type": classify_source(relpath),
                "directory": os.path.relpath(root, REPO_ROOT),
                "size_bytes": stat.st_size,
                "line_count": count_lines(filepath) if not binary else 0,
                "is_binary": binary,
                "modified_timestamp": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                "sha256_hash": get_file_hash(filepath),
            })
    return files


def build_files_table(files):
    """Table 1: Complete files inventory with all metadata."""
    schema = pa.schema([
        ("file_path", pa.string()),
        ("file_name", pa.string()),
        ("extension", pa.string()),
        ("category", pa.string()),
        ("source_type", pa.string()),
        ("directory", pa.string()),
        ("size_bytes", pa.int64()),
        ("line_count", pa.int32()),
        ("is_binary", pa.bool_()),
        ("modified_timestamp", pa.string()),
        ("sha256_hash", pa.string()),
    ])
    arrays = [
        pa.array([f["file_path"] for f in files]),
        pa.array([f["file_name"] for f in files]),
        pa.array([f["extension"] for f in files]),
        pa.array([f["category"] for f in files]),
        pa.array([f["source_type"] for f in files]),
        pa.array([f["directory"] for f in files]),
        pa.array([f["size_bytes"] for f in files], type=pa.int64()),
        pa.array([f["line_count"] for f in files], type=pa.int32()),
        pa.array([f["is_binary"] for f in files]),
        pa.array([f["modified_timestamp"] for f in files]),
        pa.array([f["sha256_hash"] for f in files]),
    ]
    return pa.table(arrays, schema=schema)


def build_extensions_summary(files):
    """Table 2: Per-extension summary statistics."""
    ext_data = {}
    for f in files:
        ext = f["extension"]
        if ext not in ext_data:
            ext_data[ext] = {
                "extension": ext,
                "category": f["category"],
                "file_count": 0,
                "total_size_bytes": 0,
                "total_lines": 0,
                "binary_count": 0,
                "text_count": 0,
                "min_size": float("inf"),
                "max_size": 0,
                "example_files": [],
            }
        d = ext_data[ext]
        d["file_count"] += 1
        d["total_size_bytes"] += f["size_bytes"]
        d["total_lines"] += f["line_count"]
        if f["is_binary"]:
            d["binary_count"] += 1
        else:
            d["text_count"] += 1
        d["min_size"] = min(d["min_size"], f["size_bytes"])
        d["max_size"] = max(d["max_size"], f["size_bytes"])
        if len(d["example_files"]) < 3:
            d["example_files"].append(f["file_path"])

    rows = sorted(ext_data.values(), key=lambda r: r["file_count"], reverse=True)

    schema = pa.schema([
        ("extension", pa.string()),
        ("category", pa.string()),
        ("file_count", pa.int32()),
        ("total_size_bytes", pa.int64()),
        ("total_lines", pa.int32()),
        ("avg_size_bytes", pa.float64()),
        ("min_size_bytes", pa.int64()),
        ("max_size_bytes", pa.int64()),
        ("binary_count", pa.int32()),
        ("text_count", pa.int32()),
        ("example_files", pa.string()),
    ])

    arrays = [
        pa.array([r["extension"] for r in rows]),
        pa.array([r["category"] for r in rows]),
        pa.array([r["file_count"] for r in rows], type=pa.int32()),
        pa.array([r["total_size_bytes"] for r in rows], type=pa.int64()),
        pa.array([r["total_lines"] for r in rows], type=pa.int32()),
        pa.array([r["total_size_bytes"] / r["file_count"] for r in rows], type=pa.float64()),
        pa.array([r["min_size"] for r in rows], type=pa.int64()),
        pa.array([r["max_size"] for r in rows], type=pa.int64()),
        pa.array([r["binary_count"] for r in rows], type=pa.int32()),
        pa.array([r["text_count"] for r in rows], type=pa.int32()),
        pa.array(["; ".join(r["example_files"]) for r in rows]),
    ]
    return pa.table(arrays, schema=schema)


def build_directories_summary(files):
    """Table 3: Per-directory summary."""
    dir_data = {}
    for f in files:
        d = f["directory"]
        if d not in dir_data:
            dir_data[d] = {
                "directory": d,
                "source_type": f["source_type"],
                "file_count": 0,
                "total_size_bytes": 0,
                "total_lines": 0,
                "extensions": set(),
                "file_list": [],
            }
        dd = dir_data[d]
        dd["file_count"] += 1
        dd["total_size_bytes"] += f["size_bytes"]
        dd["total_lines"] += f["line_count"]
        dd["extensions"].add(f["extension"])
        dd["file_list"].append(f["file_name"])

    rows = sorted(dir_data.values(), key=lambda r: r["directory"])

    schema = pa.schema([
        ("directory", pa.string()),
        ("source_type", pa.string()),
        ("file_count", pa.int32()),
        ("total_size_bytes", pa.int64()),
        ("total_lines", pa.int32()),
        ("extensions_found", pa.string()),
        ("file_list", pa.string()),
    ])

    arrays = [
        pa.array([r["directory"] for r in rows]),
        pa.array([r["source_type"] for r in rows]),
        pa.array([r["file_count"] for r in rows], type=pa.int32()),
        pa.array([r["total_size_bytes"] for r in rows], type=pa.int64()),
        pa.array([r["total_lines"] for r in rows], type=pa.int32()),
        pa.array([", ".join(sorted(r["extensions"])) for r in rows]),
        pa.array(["; ".join(r["file_list"]) for r in rows]),
    ]
    return pa.table(arrays, schema=schema)


def build_haskell_analysis(files):
    """Table 4: Deep Haskell source analysis (all .hs files)."""
    hs_files = [f for f in files if f["extension"] in (".hs", ".hs-boot")]
    if not hs_files:
        return None

    rows = []
    for f in hs_files:
        filepath = os.path.join(REPO_ROOT, f["file_path"])
        modules = []
        imports = []
        exports = []
        has_pragmas = False
        has_type_sigs = False
        pragma_list = []
        data_types = []
        class_defs = []
        instance_defs = []
        try:
            with open(filepath, "r", errors="replace") as fh:
                for line in fh:
                    stripped = line.strip()
                    if stripped.startswith("module "):
                        mod = stripped.split()[1].rstrip("(").strip()
                        if mod not in modules:
                            modules.append(mod)
                    elif stripped.startswith("import "):
                        imp = stripped.replace("import", "").strip()
                        if imp.startswith("qualified "):
                            imp = imp.replace("qualified ", "")
                        tok = imp.split()[0] if imp.split() else imp
                        if tok:
                            imports.append(tok)
                    elif stripped.startswith("{-#") and "LANGUAGE" in stripped:
                        has_pragmas = True
                        # Extract pragma names
                        inner = stripped.replace("{-#", "").replace("#-}", "").replace("LANGUAGE", "").strip()
                        for p in inner.split(","):
                            p = p.strip()
                            if p:
                                pragma_list.append(p)
                    elif stripped.startswith("{-#"):
                        has_pragmas = True
                    elif stripped.startswith("data ") or stripped.startswith("newtype "):
                        tok = stripped.split()[1] if len(stripped.split()) > 1 else ""
                        if tok:
                            data_types.append(tok)
                    elif stripped.startswith("class "):
                        tok = stripped.split()[1] if len(stripped.split()) > 1 else ""
                        if tok:
                            class_defs.append(tok)
                    elif stripped.startswith("instance "):
                        instance_defs.append(stripped.split("where")[0].replace("instance", "").strip())
                    elif "::" in stripped and not stripped.startswith("--"):
                        has_type_sigs = True
        except Exception:
            pass

        rows.append({
            "file_path": f["file_path"],
            "source_type": f["source_type"],
            "module_name": modules[0] if modules else "(unknown)",
            "line_count": f["line_count"],
            "size_bytes": f["size_bytes"],
            "import_count": len(imports),
            "imports": "; ".join(sorted(set(imports))),
            "has_language_pragmas": has_pragmas,
            "language_extensions": "; ".join(sorted(set(pragma_list))),
            "has_type_signatures": has_type_sigs,
            "data_types": "; ".join(data_types),
            "class_definitions": "; ".join(class_defs),
            "instance_count": len(instance_defs),
        })

    schema = pa.schema([
        ("file_path", pa.string()),
        ("source_type", pa.string()),
        ("module_name", pa.string()),
        ("line_count", pa.int32()),
        ("size_bytes", pa.int64()),
        ("import_count", pa.int32()),
        ("imports", pa.string()),
        ("has_language_pragmas", pa.bool_()),
        ("language_extensions", pa.string()),
        ("has_type_signatures", pa.bool_()),
        ("data_types", pa.string()),
        ("class_definitions", pa.string()),
        ("instance_count", pa.int32()),
    ])

    arrays = [
        pa.array([r["file_path"] for r in rows]),
        pa.array([r["source_type"] for r in rows]),
        pa.array([r["module_name"] for r in rows]),
        pa.array([r["line_count"] for r in rows], type=pa.int32()),
        pa.array([r["size_bytes"] for r in rows], type=pa.int64()),
        pa.array([r["import_count"] for r in rows], type=pa.int32()),
        pa.array([r["imports"] for r in rows]),
        pa.array([r["has_language_pragmas"] for r in rows]),
        pa.array([r["language_extensions"] for r in rows]),
        pa.array([r["has_type_signatures"] for r in rows]),
        pa.array([r["data_types"] for r in rows]),
        pa.array([r["class_definitions"] for r in rows]),
        pa.array([r["instance_count"] for r in rows], type=pa.int32()),
    ]
    return pa.table(arrays, schema=schema)


def build_source_type_summary(files):
    """Table 5: Summary by source classification."""
    st_data = {}
    for f in files:
        st = f["source_type"]
        if st not in st_data:
            st_data[st] = {
                "source_type": st,
                "file_count": 0,
                "total_size_bytes": 0,
                "total_lines": 0,
                "extensions": set(),
                "binary_count": 0,
                "text_count": 0,
            }
        d = st_data[st]
        d["file_count"] += 1
        d["total_size_bytes"] += f["size_bytes"]
        d["total_lines"] += f["line_count"]
        d["extensions"].add(f["extension"])
        if f["is_binary"]:
            d["binary_count"] += 1
        else:
            d["text_count"] += 1

    rows = sorted(st_data.values(), key=lambda r: r["file_count"], reverse=True)

    schema = pa.schema([
        ("source_type", pa.string()),
        ("file_count", pa.int32()),
        ("total_size_bytes", pa.int64()),
        ("total_lines", pa.int32()),
        ("binary_count", pa.int32()),
        ("text_count", pa.int32()),
        ("extensions_found", pa.string()),
    ])

    arrays = [
        pa.array([r["source_type"] for r in rows]),
        pa.array([r["file_count"] for r in rows], type=pa.int32()),
        pa.array([r["total_size_bytes"] for r in rows], type=pa.int64()),
        pa.array([r["total_lines"] for r in rows], type=pa.int32()),
        pa.array([r["binary_count"] for r in rows], type=pa.int32()),
        pa.array([r["text_count"] for r in rows], type=pa.int32()),
        pa.array([", ".join(sorted(r["extensions"])) for r in rows]),
    ]
    return pa.table(arrays, schema=schema)


def build_build_artifacts_table(files):
    """Table 6: Build artifacts and compiled outputs breakdown."""
    artifacts = [f for f in files if f["source_type"] in (
        "build_artifact", "build_cache", "package_db", "build_other"
    )]
    if not artifacts:
        return None

    schema = pa.schema([
        ("file_path", pa.string()),
        ("extension", pa.string()),
        ("category", pa.string()),
        ("source_type", pa.string()),
        ("size_bytes", pa.int64()),
        ("is_binary", pa.bool_()),
    ])

    arrays = [
        pa.array([f["file_path"] for f in artifacts]),
        pa.array([f["extension"] for f in artifacts]),
        pa.array([f["category"] for f in artifacts]),
        pa.array([f["source_type"] for f in artifacts]),
        pa.array([f["size_bytes"] for f in artifacts], type=pa.int64()),
        pa.array([f["is_binary"] for f in artifacts]),
    ]
    return pa.table(arrays, schema=schema)


def build_investigation_metadata(total_files):
    """Table 7: Investigation and project metadata."""
    schema = pa.schema([
        ("key", pa.string()),
        ("value", pa.string()),
    ])
    meta = [
        ("project_name", "squirrel-os"),
        ("project_version", "0.1.0.0"),
        ("project_language", "Haskell"),
        ("project_license", "MIT"),
        ("project_author", "xoxo"),
        ("project_maintainer", "fritsch.david@gmail.com"),
        ("build_system", "cabal"),
        ("cabal_version", "3.0"),
        ("ghc_version", "9.6.7"),
        ("ghc_base_version", "4.18.3.0"),
        ("target_platform", "x86_64-windows"),
        ("default_language", "Haskell2010"),
        ("formatter", "fourmolu"),
        ("ci_system", "GitHub Actions"),
        ("ci_runner", "ubuntu-latest"),
        ("dependency_tls_version", "2.2.0"),
        ("total_files_investigated", str(total_files)),
        ("investigation_date", datetime.now().isoformat()),
        ("investigation_tool", "pyarrow parquet generator v2"),
    ]
    arrays = [
        pa.array([m[0] for m in meta]),
        pa.array([m[1] for m in meta]),
    ]
    return pa.table(arrays, schema=schema)


def main():
    print("=== Parquet File Investigation Database Generator v2 ===")
    print("=== Now scanning ALL files including dist-newstyle ===\n")

    print("Collecting all files...")
    files = collect_all_files()
    print(f"  Found {len(files)} files total\n")

    # Count by source type
    from collections import Counter
    st_counts = Counter(f["source_type"] for f in files)
    for st, count in sorted(st_counts.items(), key=lambda x: -x[1]):
        print(f"    {st}: {count} files")
    print()

    # Count by extension
    ext_counts = Counter(f["extension"] for f in files)
    print("  Extensions found:")
    for ext, count in sorted(ext_counts.items(), key=lambda x: -x[1]):
        print(f"    {ext}: {count} files")
    print()

    # 1. Files inventory
    print("Writing files_inventory.parquet...")
    t1 = build_files_table(files)
    pq.write_table(t1, os.path.join(PARQUET_DIR, "files_inventory.parquet"))
    print(f"  {t1.num_rows} rows, {t1.num_columns} columns\n")

    # 2. Extensions summary
    print("Writing extensions_summary.parquet...")
    t2 = build_extensions_summary(files)
    pq.write_table(t2, os.path.join(PARQUET_DIR, "extensions_summary.parquet"))
    print(f"  {t2.num_rows} rows, {t2.num_columns} columns\n")

    # 3. Directories summary
    print("Writing directories_summary.parquet...")
    t3 = build_directories_summary(files)
    pq.write_table(t3, os.path.join(PARQUET_DIR, "directories_summary.parquet"))
    print(f"  {t3.num_rows} rows, {t3.num_columns} columns\n")

    # 4. Haskell analysis (all .hs files)
    print("Writing haskell_analysis.parquet...")
    t4 = build_haskell_analysis(files)
    if t4:
        pq.write_table(t4, os.path.join(PARQUET_DIR, "haskell_analysis.parquet"))
        print(f"  {t4.num_rows} rows, {t4.num_columns} columns\n")
    else:
        print("  No Haskell files found, skipping.\n")

    # 5. Source type summary
    print("Writing source_type_summary.parquet...")
    t5 = build_source_type_summary(files)
    pq.write_table(t5, os.path.join(PARQUET_DIR, "source_type_summary.parquet"))
    print(f"  {t5.num_rows} rows, {t5.num_columns} columns\n")

    # 6. Build artifacts
    print("Writing build_artifacts.parquet...")
    t6 = build_build_artifacts_table(files)
    if t6:
        pq.write_table(t6, os.path.join(PARQUET_DIR, "build_artifacts.parquet"))
        print(f"  {t6.num_rows} rows, {t6.num_columns} columns\n")
    else:
        print("  No build artifacts found, skipping.\n")

    # 7. Project metadata
    print("Writing investigation_metadata.parquet...")
    t7 = build_investigation_metadata(len(files))
    pq.write_table(t7, os.path.join(PARQUET_DIR, "investigation_metadata.parquet"))
    print(f"  {t7.num_rows} rows, {t7.num_columns} columns\n")

    print("=== All parquet files generated successfully! ===")
    print(f"=== Output: {PARQUET_DIR}/ ===")


if __name__ == "__main__":
    main()
