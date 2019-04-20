package jp.co.future.uroborosql.client.completer;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.apache.commons.lang3.StringUtils;
import org.jline.reader.LineReader;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.config.SqlConfig;

public class TableNameCompleterTest extends ReaderTestSupport {
	private final List<ReplCommand> commands = new ArrayList<>();
	private SqlConfig config;

	private SqlAgent agent;

	@Override
	@Before
	public void setUp() throws Exception {
		super.setUp();

		// ReplCommandの読み込み
		for (ReplCommand command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:TableNameCompleterTest")).build();

		agent = config.agent();

		String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}

		agent.commit();
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	public void testComplete() throws Exception {
		TableNameCompleter completer = new TableNameCompleter(commands, config.getConnectionSupplier());
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("qu", new TestBuffer("qu").tab());
		assertBuffer("generate se", new TestBuffer("generate se").tab());
		assertBuffer("generate select", new TestBuffer("generate select").tab());
		assertBuffer("generate select PRODUCT", new TestBuffer("generate select ").tab().tab());
		assertBuffer("generate select PRODUCT", new TestBuffer("generate select pro").tab().tab());

		assertBuffer("desc PRODUCT", new TestBuffer("desc PR").tab());
		assertBuffer("desc X", new TestBuffer("desc X").tab().tab());

	}

}
