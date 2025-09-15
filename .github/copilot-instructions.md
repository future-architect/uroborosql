# UroboroSQL Development Instructions

UroboroSQL is a Java SQL execution library that supports 2-way-SQL, providing a SQL-centered design approach for Java 11+ applications. The library includes comprehensive database support, testing frameworks, and an interactive REPL for SQL development.

**Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.**

## Working Effectively

### Prerequisites and Setup
- Requires Java 11 or later (Java 17 is currently available and works perfectly)
- Requires Maven 3.x (Maven 3.9.11 is available)
- No additional setup scripts required - Maven handles all dependencies

### Building the Project
- **Clean and compile:** `mvn clean compile -q --no-transfer-progress`
  - Takes approximately 6-32 seconds. NEVER CANCEL. Set timeout to 5+ minutes.
- **Package (no tests):** `mvn clean package -DskipTests -Dmaven.javadoc.skip=true -q --no-transfer-progress`
  - Takes approximately 15 seconds. NEVER CANCEL. Set timeout to 5+ minutes.
- **Full build with tests:** `mvn clean test -q --no-transfer-progress`
  - Takes approximately 1-2 minutes. NEVER CANCEL. Set timeout to 10+ minutes.
  - **NOTE:** Some timezone-related tests may fail in containerized environments - this is expected
- **License formatting:** `mvn license:format -q --no-transfer-progress`
  - Takes approximately 3 seconds. Required before committing changes.

### Testing
- **Run all tests:** `mvn test -q --no-transfer-progress`
  - Takes approximately 1-2 minutes. NEVER CANCEL. Set timeout to 10+ minutes.
- **Run single test class:** `mvn test -Dtest=SqlFunctionTest -q --no-transfer-progress`
  - Takes approximately 14 seconds. NEVER CANCEL. Set timeout to 2+ minutes.
- **Database profile testing:** Available profiles: h2 (default), postgresql, oracle, sqlserver, mysql, mysql8
  - Example: `mvn test -Ph2 -q --no-transfer-progress`
- **CI matrix testing:** `mvn test -P<db_profile> -Dgroups=matrix --no-transfer-progress`

### REPL (Interactive SQL Development)
- **Start REPL:** `mvn -P REPL -q --no-transfer-progress`
- **Configuration:** Located in `REPL/repl.properties`
- **Default database:** H2 in-memory database
- **REPL commands:**
  - `query [sql_file_name]` - Execute query from loaded SQL file
  - `update [sql_file_name]` - Execute update from loaded SQL file
  - `view [sql_file_name]` - View SQL file contents
  - `list` - List loaded SQL files
- **Exit REPL:** Use Ctrl+C or `exit` command

### Code Quality and Validation
- **ALWAYS run license formatting before committing:** `mvn license:format -q --no-transfer-progress`
- **No additional linting tools configured** - use standard Java formatting
- **Coverage analysis:** Built into test execution with `uroborosql.sql.coverage=true`

## Project Structure and Navigation

### Key Components
- **Core library:** `src/main/java/jp/co/future/uroborosql/` (285 Java files)
- **Main entry point:** `src/main/java/jp/co/future/uroborosql/UroboroSQL.java`
- **SQL Agent:** Primary interface for SQL execution
- **Fluent API:** Located in `src/main/java/jp/co/future/uroborosql/fluent/`
- **2-way SQL examples:** `src/test/resources/sql/example/` directory

### Important Directories
- **SQL files:** `src/test/resources/sql/` - Contains 2-way SQL examples and test cases
- **REPL configuration:** `REPL/repl.properties`
- **Database configurations:** Multiple database support with profiles
- **Test resources:** `src/test/resources/` (200 test Java files)

### 2-Way SQL Examples
Located in `src/test/resources/sql/example/`:
- `select_product.sql` - Basic product selection with conditional parameters
- `insert_product.sql` - Product insertion with parameter binding
- `update_product.sql` - Product updates
- `select_test.sql` - Conditional WHERE clauses with IF statements

## Validation and Testing Scenarios

### Manual Validation Requirements
After making changes, **ALWAYS** perform these validation steps:

1. **Build validation:**
   ```bash
   mvn clean compile -q --no-transfer-progress
   ```

2. **License validation:**
   ```bash
   mvn license:format -q --no-transfer-progress
   ```

3. **Basic functionality test:**
   ```bash
   mvn test -Dtest=SqlFunctionTest -q --no-transfer-progress
   ```

4. **REPL functionality test:**
   ```bash
   # Start REPL and verify it loads
   timeout 30 mvn -P REPL -q --no-transfer-progress
   ```

### Integration Testing
- **Test SQL parsing:** Use REPL to load and view example SQL files
- **Test database connectivity:** Run single test with H2 profile
- **Test 2-way SQL functionality:** Load and execute SQL files in REPL

## Common Tasks and Expected Timing

### Build Commands with Timeouts
- `mvn clean compile` - **6-32 seconds** (NEVER CANCEL, timeout: 5+ minutes)
- `mvn clean package -DskipTests -Dmaven.javadoc.skip=true` - **15 seconds** (NEVER CANCEL, timeout: 5+ minutes)
- `mvn test` - **1-2 minutes** (NEVER CANCEL, timeout: 10+ minutes)
- `mvn license:format` - **3 seconds** (NEVER CANCEL, timeout: 2+ minutes)
- Single test execution - **14 seconds** (NEVER CANCEL, timeout: 2+ minutes)

### Database Profiles
- **Default:** H2 (in-memory)
- **Available:** postgresql, oracle, sqlserver, mysql, mysql8
- **Usage:** Add `-P<profile_name>` to Maven commands

### Debugging and Development
- **View SQL examples:** Check `src/test/resources/sql/example/` for 2-way SQL patterns
- **REPL for testing:** Use `mvn -P REPL` for interactive SQL development
- **Log files:** Coverage logs written to `target/coverage.log`

## Known Issues and Workarounds
- **Javadoc generation fails** due to network restrictions - skip with `-Dmaven.javadoc.skip=true`
- **Some timezone tests fail** in containerized environments - this is expected and does not affect functionality
- **REPL creates dumb terminal** warning - this is expected in headless environments but functionality works

## Quick Reference Commands
```bash
# Standard development cycle
mvn clean compile -q --no-transfer-progress
mvn license:format -q --no-transfer-progress
mvn test -Dtest=SqlFunctionTest -q --no-transfer-progress

# Full build and test (before PR)
mvn clean package -DskipTests -Dmaven.javadoc.skip=true -q --no-transfer-progress
mvn test -q --no-transfer-progress

# Interactive development
mvn -P REPL -q --no-transfer-progress
```

## Repository Statistics
- **Main Java files:** 285
- **Test Java files:** 200
- **SQL example files:** 15+ in `src/test/resources/sql/example/`
- **Database support:** H2, PostgreSQL, Oracle, SQL Server, MySQL
- **Java version:** 11+ (tested with 17)
- **Build tool:** Maven 3.x