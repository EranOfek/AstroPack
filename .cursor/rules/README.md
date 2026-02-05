
https://chatgpt.com/c/697ffeeb-ab68-8395-ab98-285f730ae920


# Using Claude Code to learn AstroPack

https://chatgpt.com/c/698081e9-ce88-838a-ba1a-76142e2f37dd


# Claude Code - Prompt 1

PROMPT START

You are acting as a senior MATLAB architect onboarding into a large, mature scientific codebase.

Your task is to systematically learn and distill the codebase located in the matlab/ directory only.

Scope rules (MANDATORY)

Analyze only files under matlab/

Ignore all other folders (tests/, data/, external/, etc.)

Treat this as the authoritative source code of the system

What this codebase likely represents

A long-lived scientific MATLAB framework

Domain: astrophysics / astronomy

Emphasis on correctness, reproducibility, and numerical reliability

Phase 1 — Silent ingestion

First, recursively scan the entire matlab/ directory.

Do not summarize yet.
Do not propose changes.
Just build an internal mental model.

Phase 2 — Architecture extraction

Produce a structured explanation covering:

High-level architecture

Major subsystems / modules

How they relate

Layering (core utilities vs domain logic vs interfaces)

Core abstractions

What are the central concepts (objects, structs, function families)

Which folders define foundational primitives

What is considered “core” vs “helper”

Data flow

Typical flow of data through the system

Common data structures and conventions

How metadata, numeric arrays, and files are handled

Phase 3 — MATLAB-specific conventions

Extract implicit rules, including:

Naming conventions (functions, folders, variables)

Error handling patterns

Use of MATLAB OOP vs procedural style

File layout conventions

Common idioms and anti-patterns

Phase 4 — Public vs internal API

Infer:

What is meant to be used by external users

What is internal / private

How boundaries are enforced (or not)

Phase 5 — Output format (VERY IMPORTANT)

Your final output must be clean, stable documentation, suitable to be saved as project knowledge files.

Produce the following sections in this order:

Architecture Overview

Core Concepts and Abstractions

Module / Folder Responsibilities

MATLAB Coding Conventions

Implicit Design Rules

Anti-Patterns and Things to Avoid

Glossary of Project-Specific Terms

Write clearly, concisely, and factually.
Do not hallucinate missing intent — infer only from code evidence.

Assume this document will be used later to guide Cursor-based development.

PROMPT END
