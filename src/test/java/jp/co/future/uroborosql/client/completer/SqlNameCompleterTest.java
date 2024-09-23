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
import jp.co.future.uroborosql.store.SqlResourceManager;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;

public class SqlNameCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();
	private static SqlResourceManager sqlManager;

	@BeforeAll
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (var command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

		sqlManager = new SqlResourceManagerImpl();
		sqlManager.setDialect(new DefaultDialect());
		sqlManager.initialize();
		List.of("ddl/create_tables",
				"example/insert_column_type_test",
				"example/insert_product",
				"example/insert_product_for_bean",
				"example/insert_product_for_optional",
				"example/insert_product_regist_work",
				"example/select_column_type_test",
				"example/select_consts",
				"example/select_enum",
				"example/select_product",
				"example/select_product_param_camel",
				"example/select_product_where_upd_datetime",
				"example/select_test",
				"example/selectinsert_product",
				"example/selectinsert_product2")
				.forEach(sqlName -> sqlManager.getSql(sqlName));
	}

	@Test
	void testComplete() throws Exception {
		var completer = new SqlNameCompleter(commands, sqlManager);
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
