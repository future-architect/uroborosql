package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.store.NioSqlResourceManagerImpl;
import jp.co.future.uroborosql.store.SqlResourceManager;

public class SqlNameCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();
	private static SqlResourceManager sqlResourceManager;

	@BeforeAll
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (ReplCommand command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

		sqlResourceManager = new NioSqlResourceManagerImpl(false);
		sqlResourceManager.setDialect(new DefaultDialect());
		sqlResourceManager.initialize();
	}

	@Test
	public void testComplete() throws Exception {
		var completer = new SqlNameCompleter(commands, sqlResourceManager);
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("qu", new TestBuffer("qu").tab());
		assertBuffer("query ddl/create_tables ", new TestBuffer("query dd").tab());
		assertBuffer("query example/select_product", new TestBuffer("query example/select_p").tab());
		assertBuffer("query example/insert_product", new TestBuffer("query example").tab().tab().tab());
		assertBuffer("query example/select_product", new TestBuffer("query example/select_product").tab());
		assertBuffer("query example/select_product ", new TestBuffer("query example/select_product ").tab());
		assertBuffer("query example/select_product p",
				new TestBuffer("query example/select_product p").left().left().back().tab());

	}

}
