[![Maven Central](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg?style=plastic)](https://raw.githubusercontent.com/future-architect/uroborosql/master/LICENSE)

uroboroSQL
==========

<img src="https://future-architect.github.io/uroborosql-doc//images/logo.png" style="max-width: 600px;" alt="uroboroSQL" />

UroboroSQL is a simple SQL execution library that can use 2-way-SQL compatible with Java8.

UroboroSQL mainly adopts SQL-centered design concept. It is not an idea of ​​assembling SQL thoughtfully focusing on Java, but an idea of ​​supplementing Java with missing SQL.

As know-how cultivated in the enterprise, we are enhancing functions such as partition value support, retry, filter customization and so on. It also features a function from a quality viewpoint that makes coverage available for 2Way-SQL.

for Japanese, [README.ja.md](https://github.com/future-architect/uroborosql/blob/master/README.ja.md)

Installation
------------

#### for Maven

```xml
<dependency>
    <groupId>jp.co.future</groupId>
    <artifactId>uroborosql</artifactId>
    <version>0.1.0</version>
</dependency>
```

#### for Gradle

```gradle
compile group: jp.co.future, name: uroborosql, version: 0.1.0
```

Documentation
-------------

[https://future-architect.github.io/uroborosql-doc/](https://future-architect.github.io/uroborosql-doc/)

Requirement
-----------

- Java 1.8 or later.

Quick start
-----------

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
SqlConfig config = DefaultSqlConfig.getConfig("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "");

try (SqlAgent agent = config.createAgent()) {
  // SELECT
  List<Map<String, Object>> departments = agent.query("department/select_department").param("dept_no", 1001).collect();

  // INSERT
  int count = agent.update("department/insert_department")
    .param("dept_no", 1001)
    .param("dept_name", "sales")
    .count();
}

```

Sample application
------------------

https://github.com/future-architect/uroborosql-sample

License
-------

Released under the [MIT License](https://github.com/future-architect/uroborosql/blob/master/LICENSE).
