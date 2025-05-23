"""
Parser for maven_dep_tree.txt files, generated by maven
Based on the output of this maven plugin https://maven.apache.org/plugins/maven-dependency-plugin/tree-mojo.html
"""
from pathlib import Path
from typing import Any
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import consume_line
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import mark_line
from semdep.parsers.util import ParsedDependency
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity

# Annoying to read. In english "6 colon separated strings or 5 colon separated strings"
# Examples:
# org.apache.logging.log4j:log4j-api:jar:0.0.2:compile
# org.springframework.boot:spring-boot-configuration-processor:jar:2.3.4.RELEASE:compile (optional)
# com.google.inject:guice:jar:no_aop:4.2.2:test
dep = regex(
    "([^:\n]+:[^:\n]+):[^:\n]+:[^:\n]+:([^:\n]+):[^:\n]+", flags=0, group=(1, 2)
) | regex("([^:\n]+:[^:\n]+):[^:\n]+:([^:\n]+):[^:\n]+", flags=0, group=(1, 2))


# Examples (these would not appear in this order in a file, they're separate):
# |  +- org.apache.maven:maven-model:jar:3.8.6:provided

# |  |  \- org.codehaus.plexus:plexus-component-annotations:jar:1.5.5:provided

# +- org.apache.logging.log4j:log4j-api:jar:0.0.2:compile

#    \- net.java.dev.jna:jna:jar:5.11.0:compile

# |     +- org.springframework:spring-aop:jar:5.3.9:compile
tree_line = mark_line(
    regex(r"((\|  )|(   ))*").bind(
        lambda depth: (regex("(\\+- )|(\\\\- )"))
        >> dep.map(
            lambda d: {
                "line_number": 0,
                "depth": len(depth) // 3,
                "transitivity": Transitivity(
                    Transitive() if len(depth) // 3 > 0 else Direct()
                ),
                "children": [],
                "package": d[0],
                "version": d[1],
            }
        )
        # ignore lines that we don't recognize
        | consume_line
    )
)


pom_tree = (
    consume_line  # First line is the name of the current project, ignore it
    >> string("\n")
    >> tree_line.sep_by(string("\n"))
    << string("\n").optional()
)


def get_children(deps: List[Any]) -> List[ParsedDependency]:
    stack: List[Any] = []
    results = []
    for line_number, dep in deps:
        if dep is None:
            continue
        dep["line_number"] = line_number
        if not stack:
            stack.append(dep)
            continue
        if dep["depth"] == stack[-1]["depth"]:
            results.append(ParsedDependency.from_dict(stack.pop()))
            if stack:
                child = DependencyChild(package=dep["package"], version=dep["version"])
                stack[-1]["children"].append(child)
            stack.append(dep)
        elif dep["depth"] > stack[-1]["depth"]:
            child = DependencyChild(package=dep["package"], version=dep["version"])
            stack[-1]["children"].append(child)
            stack.append(dep)
        else:
            while len(stack) > 0 and dep["depth"] <= stack[-1]["depth"]:
                results.append(ParsedDependency.from_dict(stack.pop()))
            if stack:
                child = DependencyChild(package=dep["package"], version=dep["version"])
                stack[-1]["children"].append(child)
            stack.append(dep)

    while len(stack) > 0:
        results.append(ParsedDependency.from_dict(stack.pop()))
    return results


def parse_pom_tree(
    tree_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(tree_path, pom_tree, ScaParserName(out.PPomtree())),
        None,
    )
    if not parsed_lockfile:
        return [], errors
    output = []
    seen_matches = set()
    deps_with_children = get_children(parsed_lockfile)
    for match in deps_with_children:
        if match is None:
            continue

        if match in seen_matches:
            continue
        seen_matches.add(match)

        output.append(
            FoundDependency(
                package=match.package,
                version=match.version,
                ecosystem=Ecosystem(Maven()),
                allowed_hashes={},
                transitivity=match.transitivity,
                line_number=match.line_number,
                children=match.children,
                lockfile_path=Fpath(str(tree_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output, errors
