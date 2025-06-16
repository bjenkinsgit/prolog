# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Running Prolog Programs
- Use SWI-Prolog: `swipl filename.pl` to load and run Prolog files
- For interactive queries: `swipl -t "goal."` to run a specific goal
- For HTTP server example: `swipl hello_server.pl` (runs on port 8000)

### Testing
- Exercism problems: `./test_exercism.sh` (tests connectivity to exercism.org)
- Individual Prolog tests: Load file in SWI-Prolog and run queries manually

### Python Integration
- Kitchen optimizer uses PySwip: `python3 kitchen/restaurant_optimizer.py`
- Requires: `pip install pyswip openai` (PySwip connects Python to SWI-Prolog)

## Architecture

### Project Structure
- **exercism/**: Constraint satisfaction problems (zebra puzzle, etc.)
- **kitchen/**: Restaurant optimization using Prolog + OpenAI integration
- **learnprolognow/**: Learning exercises from Learn Prolog Now tutorial
- **AdaptiveThinkingBook/**: Examples from adaptive thinking/AI books
- **book/**: Logic programming examples and recursion patterns

### Key Integration Patterns
1. **HTTP/JSON APIs**: `hello_server.pl` shows SWI-Prolog HTTP server setup
2. **OpenAI Integration**: `gptai.pl` and `restaurant_optimizer.py` demonstrate LLM+Prolog workflows
3. **Constraint Logic Programming**: Uses `library(clpfd)` for optimization problems (kitchen scheduling)
4. **Python-Prolog Bridge**: PySwip library enables Python to call Prolog predicates

### File Naming Conventions
- `.pl` files are Prolog source code
- Versioned experiments use `v7.pl`, `v8.pl` pattern
- Test scripts use `test_*.sh` pattern
- API key stored in `key` file (not committed)