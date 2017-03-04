package jp.co.future.uroborosql.parser;

import java.util.Stack;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.exception.EndCommentNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.IfConditionNotFoundRuntimeException;
import jp.co.future.uroborosql.node.BeginNode;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.ContainerNode;
import jp.co.future.uroborosql.node.ElseNode;
import jp.co.future.uroborosql.node.EmbeddedValueNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.ParenBindVariableNode;
import jp.co.future.uroborosql.node.PrefixSqlNode;
import jp.co.future.uroborosql.node.SqlNode;

import org.apache.commons.lang3.StringUtils;

/**
 * SQL解析処理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlParserImpl implements SqlParser {
	/** SQLトークナイザ */
	private final SqlTokenizer tokenizer;

	/** ノードのスタック */
	private final Stack<Node> nodeStack = new Stack<>();

	/** 終端文字削除用の正規表現 */
	private static final Pattern PATTERN = Pattern.compile(";$");

	/**
	 * コンストラクタ
	 *
	 * @param sql パース対象SQL
	 */
	public SqlParserImpl(final String sql) {
		this(sql, true);
	}

	/**
	 * SqlParserImplのコンストラクタ
	 *
	 * @param sql パースするSQL
	 * @param removeTerminator 終端文字（;）を除去するかどうか
	 */
	public SqlParserImpl(final String sql, final boolean removeTerminator) {
		String s = sql.trim();
		if (removeTerminator) {
			s = PATTERN.matcher(s).replaceFirst("");
		}
		tokenizer = new SqlTokenizerImpl(s);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlParser#parse()
	 */
	@Override
	public ContextTransformer parse() {
		push(new ContainerNode());
		while (TokenType.EOF != tokenizer.next()) {
			parseToken();
		}
		return new ContextTransformer(pop());
	}

	/**
	 * トークン解析
	 */
	@SuppressWarnings("incomplete-switch")
	protected void parseToken() {
		switch (tokenizer.getTokenType()) {
		case SQL:
			parseSql();
			break;
		case COMMENT:
			parseComment();
			break;
		case ELSE:
			parseElse();
			break;
		case BIND_VARIABLE:
			parseBindVariable();
			break;
		}
	}

	/**
	 * SQL解析
	 */
	protected void parseSql() {
		String sql = tokenizer.getToken();
		Node node = peek();
		if ((node instanceof IfNode || node instanceof ElseNode) && node.getChildSize() == 0) {

			SqlTokenizer st = new SqlTokenizerImpl(sql);
			st.skipWhitespace();
			String token = st.skipToken();
			st.skipWhitespace();
			if ("AND".equalsIgnoreCase(token) || "OR".equalsIgnoreCase(token)) {
				node.addChild(new PrefixSqlNode(st.getBefore(), st.getAfter()));
			} else {
				node.addChild(new SqlNode(sql));
			}
		} else {
			node.addChild(new SqlNode(sql));
		}
	}

	/**
	 * コメント解析
	 */
	protected void parseComment() {
		String comment = tokenizer.getToken();
		if (isTargetComment(comment)) {
			if (isIfComment(comment)) {
				parseIf();
			} else if (isElIfComment(comment)) {
				parseElIf();
			} else if (isElseComment(comment)) {
				parseElse();
			} else if (isBeginComment(comment)) {
				parseBegin();
			} else if (isEndComment(comment)) {
				return;
			} else {
				parseCommentBindVariable();
			}
		} else {
			parseNormalComment();
		}
	}

	/**
	 * 通常コメント解析
	 */
	protected void parseNormalComment() {
		String comment = tokenizer.getToken();
		SqlNode node = new SqlNode("/*" + comment + "*/");
		peek().addChild(node);
	}

	/**
	 * IF文解析
	 */
	protected void parseIf() {
		String condition = tokenizer.getToken().substring(2).trim();
		if (StringUtils.isEmpty(condition)) {
			throw new IfConditionNotFoundRuntimeException();
		}
		IfNode ifNode = new IfNode(condition);
		peek().addChild(ifNode);
		push(ifNode);
		parseEnd();
	}

	/**
	 * IF文解析
	 */
	protected void parseElIf() {
		String condition = tokenizer.getToken().substring(4).trim();
		if (StringUtils.isEmpty(condition)) {
			throw new IfConditionNotFoundRuntimeException();
		}
		IfNode elifNode = new IfNode(condition);
		IfNode ifNode = (IfNode) pop();
		ifNode.setElseIfNode(elifNode);
		push(elifNode);

		// parseEnd();
	}

	/**
	 * BEGIN文解析
	 */
	protected void parseBegin() {
		BeginNode beginNode = new BeginNode();
		peek().addChild(beginNode);
		push(beginNode);
		parseEnd();
	}

	/**
	 * END文解析
	 */
	protected void parseEnd() {
		while (TokenType.EOF != tokenizer.next()) {
			if (tokenizer.getTokenType() == TokenType.COMMENT && isEndComment(tokenizer.getToken())) {

				pop();
				return;
			}
			parseToken();
		}
		throw new EndCommentNotFoundRuntimeException();
	}

	/**
	 * ELSE文解析
	 */
	protected void parseElse() {
		Node parent = peek();
		if (!(parent instanceof IfNode)) {
			return;
		}
		IfNode ifNode = (IfNode) pop();
		ElseNode elseNode = new ElseNode();
		ifNode.setElseNode(elseNode);
		push(elseNode);
		tokenizer.skipWhitespace();
	}

	/**
	 * バインド変数解析
	 */
	protected void parseCommentBindVariable() {
		String expr = tokenizer.getToken();
		String s = tokenizer.skipToken();
		if (s.startsWith("(") && s.endsWith(")")) {
			peek().addChild(new ParenBindVariableNode(expr));
		} else if (expr.startsWith("#")) {
			peek().addChild(new EmbeddedValueNode(expr.substring(1), true));
		} else if (expr.startsWith("$")) {
			peek().addChild(new EmbeddedValueNode(expr.substring(1)));
		} else {
			peek().addChild(new BindVariableNode(expr));
		}
	}

	/**
	 * バインド変数解析
	 */
	protected void parseBindVariable() {
		String expr = tokenizer.getToken();
		peek().addChild(new BindVariableNode(expr));
	}

	/**
	 * ノードPOP
	 *
	 * @return Nodeオブジェクト
	 */
	protected Node pop() {
		return nodeStack.pop();
	}

	/**
	 * ノードPEEK
	 *
	 * @return Nodeオブジェクト
	 */
	protected Node peek() {
		return nodeStack.peek();
	}

	/**
	 * ノードPUSH
	 *
	 * @return Nodeオブジェクト
	 */
	protected void push(final Node node) {
		nodeStack.push(node);
	}

	/**
	 * ELSE判定
	 *
	 * @return
	 */
	protected boolean isElseMode() {
		for (int i = 0; i < nodeStack.size(); ++i) {
			if (nodeStack.get(i) instanceof ElseNode) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 解析対象コメント判定
	 *
	 * @param comment
	 * @return 解析対象の場合は<code>true</code>
	 */
	private static boolean isTargetComment(final String comment) {
		return comment != null && comment.length() > 0
				&& (Character.isJavaIdentifierStart(comment.charAt(0)) || '#' == comment.charAt(0));
	}

	/**
	 * IFコメント判定
	 *
	 * @param comment
	 * @return IFコメントの場合は<code>true</code>
	 */
	private static boolean isIfComment(final String comment) {
		return comment.startsWith("IF");
	}

	/**
	 * ELSEIFコメント判定
	 *
	 * @param comment
	 * @return ELSEIFコメントの場合は<code>true</code>
	 */
	private static boolean isElIfComment(final String comment) {
		return comment.startsWith("ELIF");
	}

	/**
	 * ELSEコメント判定
	 *
	 * @param comment
	 * @return ELSEコメントの場合は<code>true</code>
	 */
	private static boolean isElseComment(final String comment) {
		return comment.startsWith("ELSE");
	}

	/**
	 * BEGINコメント判定
	 *
	 * @param content
	 * @return BEGINコメントの場合は<code>true</code>
	 */
	private static boolean isBeginComment(final String content) {
		return content != null && "BEGIN".equals(content);
	}

	/**
	 * ENDコメント判定
	 *
	 * @param content
	 * @return ENDコメントの場合は<code>true</code>
	 */
	private static boolean isEndComment(final String content) {
		return content != null && "END".equals(content);
	}

}
