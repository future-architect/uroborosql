[![Maven Central](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg?style=plastic)](https://raw.githubusercontent.com/future-architect/uroborosql/master/LICENSE) [![Javadocs](https://www.javadoc.io/badge/jp.co.future/uroborosql.svg)](https://www.javadoc.io/doc/jp.co.future/uroborosql) [![Build Status](https://travis-ci.org/future-architect/uroborosql.svg?branch=master)](https://travis-ci.org/future-architect/uroborosql) [![Coverage Status](https://coveralls.io/repos/github/future-architect/uroborosql/badge.svg?branch=master)](https://coveralls.io/github/future-architect/uroborosql?branch=master)

uroboroSQL
==========

<img src="https://future-architect.github.io/uroborosql-doc//images/logo.png" style="max-width: 600px;" alt="uroboroSQL" />

UroboroSQL is a simple SQL execution library that can utilize 2-way-SQL compatible with Java 8.

UroboroSQL mainly adopts a SQL-centered design concept. The concept does not focus on Java to assemble SQL, but is based on an idea of making up for weaknesses of SQL with Java.

From the knowhow acquired through our business, we have enhanced UroboroSQL with functions like partition value support, retrying, filtering customization, and so on. Also, for quality assurance purposes, it features a coverage analysis function available for 2-way-SQL.

There is also REPL which dramatically increases SQL development productivity.

[![asciicast](https://asciinema.org/a/122312.png)](https://asciinema.org/a/122312)

for Japanese, see [README.ja.md](https://github.com/future-architect/uroborosql/blob/master/README.ja.md)

Installation
------------

#### for Maven

```xml
<dependency>
    <groupId>jp.co.future</groupId>
    <artifactId>uroborosql</artifactId>
    <version>0.17.0</version>
</dependency>
```

#### for Gradle

```gradle
compile group: 'jp.co.future', name: 'uroborosql', version: '0.17.0'
```

Documentation
-------------

[https://future-architect.github.io/uroborosql-doc/](https://future-architect.github.io/uroborosql-doc/)

Requirement
-----------

-	Java 1.8 or later.

Quick start
-----------

### 2Way-SQL

```sql
/* department/select_department.sql */

SELECT /* _SQL_ID_ */
  DEPT.DEPT_NO  AS  DEPT_NO
, DEPT.DEPT_NAME  AS  DEPT_NAME
FROM
  DEPARTMENT  DEPT
WHERE
  1       = 1
/*IF SF.isNotEmpty(dept_no)*/
AND DEPT.DEPT_NO  = /*dept_no*/1
/*END*/
/*IF SF.isNotEmpty(dept_name)*/
AND DEPT.DEPT_NAME  = /*dept_name*/'sample'
/*END*/

```

```sql
/* department/insert_department.sql */

INSERT
/* _SQL_ID_ */
INTO
  DEPARTMENT
(
  DEPT_NO
, DEPT_NAME
) VALUES (
  /*dept_no*/1
, /*dept_name*/'sample'
)
```

```java
SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "").build();

try (SqlAgent agent = config.agent()) {
  // SELECT
  List<Map<String, Object>> departments = agent.query("department/select_department").param("dept_no", 1001).collect();

  // INSERT
  int count = agent.update("department/insert_department")
    .param("dept_no", 1001)
    .param("dept_name", "sales")
    .count();
}
```

### DAO Interface

```java
SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "").build();

try (SqlAgent agent = config.agent()) {
  // select
  Department dept =
      agent.find(Department.class, 1001).orElse(null);

  // insert
  Department hrDept = new Department();
  hrDept.setDeptNo(1002);
  hrDept.setDeptName("HR");
  agent.insert(hrDept);

  // update
  hrDept.setDeptName("Human Resources");
  agent.update(hrDept);

  // delete
  agent.delete(hrDept);
}
```

Sample application
------------------

- CLI
    - https://github.com/future-architect/uroborosql-sample
- Web application(with Spring Boot)
    - https://github.com/shout-star/uroborosql-springboot-demo

Automated code generation
-------------------------

- uroboroSQL source generator
    - https://github.com/shout-star/uroborosql-generator

SQL Formatter
-------------

<img src="https://github.com/future-architect/uroboroSQL-formatter/raw/master/image/uroboroSQLformatter_logo.png" style="max-width: 500px;" alt="uroboroSQL" />

We also prepare a SQL formatter useful for development.

-	CLI
	-	https://github.com/future-architect/uroboroSQL-formatter
-	Sublime Text 3 Plugin
	-	https://github.com/future-architect/Sublime-uroboroSQL-formatter
-	IntelliJ IDEA Platform Plugin
	-	https://github.com/future-architect/idea-uroborosql-formatter
-	Eclipse Plugin
	-	https://github.com/future-architect/eclipse-uroborosql-formatter

License
-------

Released under the [MIT License](https://github.com/future-architect/uroborosql/blob/master/LICENSE).
