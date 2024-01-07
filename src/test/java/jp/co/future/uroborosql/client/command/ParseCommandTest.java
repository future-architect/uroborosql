package jp.co.future.uroborosql.client.command;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.DriverManager;
import java.util.List;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContextProviderImpl;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;

public class ParseCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private ReplCommand command;

	@Override
	@BeforeEach
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setExecutionContextProvider(new ExecutionContextProviderImpl()
						.setConstantClassNames(List.of("jp.co.future.uroborosql.context.test.TestConsts"))
						.setEnumConstantPackageNames(List.of("jp.co.future.uroborosql.context.test")))
				.setSqlResourceManager(new SqlResourceManagerImpl())
				.build();

		command = new ParseCommand();
	}

	@AfterEach
	public void tearDown() throws Exception {
	}

	@Test
	void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		var sqlName = "test/PARSE_TEST";
		var commandLine = "parse" + " " + sqlName;
		var flag = command.execute(reader, commandLine.split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("SQL :");

		var sqlLine = sqlConfig.getSqlResourceManager().getSql(sqlName).split("\\r\\n|\\r|\\n");
		for (var line : sqlLine) {
			assertConsoleOutputContains(line);
		}

		assertConsoleOutputContains("BRANCHES :");
		assertConsoleOutputContains("\tBEGIN {");
		assertConsoleOutputContains("\t\tIF ( param11 != null ) {");
		assertConsoleOutputContains("\t\t} ELIF ( param11 == null and param12 != null ) {");
		assertConsoleOutputContains("\t\t} ELSE {");
		assertConsoleOutputContains("\t\t\tIF ( SF.isNotEmpty(param21) ) {");
		assertConsoleOutputContains(
				"\t\t\t} ELIF ( SF.isEmpty(param21) and SF.isNotEmpty(param22) or SF.isNotEmpty(param23) ) {");
		assertConsoleOutputContains("\t\t\t}");
		assertConsoleOutputContains("\t\t}");
		assertConsoleOutputContains("\t}");
		assertConsoleOutputContains("BIND_PARAMS :");
		assertConsoleOutputContains("\tTABLE_NAME");
		assertConsoleOutputContains("\tparam11");
		assertConsoleOutputContains("\tparam12");
		assertConsoleOutputContains("\tparam21");
		assertConsoleOutputContains("\tparam22");
		assertConsoleOutputContains("\tparam23");
		assertConsoleOutputContains("\tCLS_LONG (1)");
		assertConsoleOutputContains("\tCLS_SQL_TIMESTAMP (1970-01-01 09:00:00.003)");
		assertConsoleOutputContains("\tCLS_STRING ('AAA')");
		assertConsoleOutputContains("\tCLS_TEST_ENUM1_A (A)");
		assertConsoleOutputContains("\tCLS_TEST_ENUM1_D (not found)");
	}

	@Test
	void testExecuteNotFound() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		var flag = command.execute(reader, "parse test/NOTFOUND".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("sqlName : test/NOTFOUND not found.");
	}

	@Test
	void testExecuteNotArgument() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		var flag = command.execute(reader, "parse".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("sqlName must be specified.");
	}

	@Test
	void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("parse sql file.");
		assertConsoleOutputContains("ex) parse [sql file name]<Enter> : Parse sql file.");
	}

}
