[![Maven Central](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/jp.co.future/uroborosql) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg?style=plastic)](https://raw.githubusercontent.com/future-architect/uroborosql/master/LICENSE) [![Javadocs](https://www.javadoc.io/badge/jp.co.future/uroborosql.svg)](https://www.javadoc.io/doc/jp.co.future/uroborosql)

uroboroSQL
==========

<img src="https://future-architect.github.io/uroborosql-doc//images/logo.png" style="max-width: 600px;" alt="uroboroSQL" />

uroboroSQLは、Java8対応の2Way-SQLが利用可能なシンプルなSQL実行ライブラリです。

uroboroSQLは主にSQL中心の設計コンセプトを採用しています。Javaを中心に考えてSQLを組み立てるという思想ではなく、SQLに足りないところをJavaで補うという思想です。

エンタープライズで培われたノウハウとして、区分値サポート、リトライ、フィルターによるカスタマイズなどの機能を充実させています。また、2Way-SQLに対して、カバレッジを取れるようにするという、品質視点での機能があるのも特徴です。

さらにSQLの開発生産性を格段に上げるREPL機能も搭載しています。

[![asciicast](https://asciinema.org/a/122312.png)](https://asciinema.org/a/122312)

Installation
------------

#### for Maven

```xml
<dependency>
    <groupId>jp.co.future</groupId>
    <artifactId>uroborosql</artifactId>
    <version>0.2.0</version>
</dependency>
```

#### for Gradle

```gradle
compile group: 'jp.co.future', name: 'uroborosql', version: '0.2.0'
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
	DEPT.DEPT_NO	AS	DEPT_NO
,	DEPT.DEPT_NAME	AS	DEPT_NAME
FROM
	DEPARTMENT	DEPT
WHERE
	1				=	1
/*IF SF.isNotEmpty(dept_no)*/
AND	DEPT.DEPT_NO	=	/*dept_no*/1
/*END*/
/*IF SF.isNotEmpty(dept_name)*/
AND	DEPT.DEPT_NAME	=	/*dept_name*/'sample'
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
,	DEPT_NAME
) VALUES (
	/*dept_no*/1
,	/*dept_name*/'sample'
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

### DAO Interface

```java
SqlConfig config = DefaultSqlConfig.getConfig("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "");

try (SqlAgent agent = config.createAgent()) {
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

https://github.com/future-architect/uroborosql-sample

SQL Formatter
-------------

<img src="https://github.com/future-architect/uroboroSQL-formatter/raw/master/image/uroboroSQLformatter_logo.png" style="max-width: 500px;" alt="uroboroSQL" />

開発で便利なSQL formatterも用意しています。

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
