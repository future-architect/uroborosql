/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parser;

import java.util.Stack;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.exception.EndCommentNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.IfConditionNotFoundRuntimeException;
import jp.co.future.uroborosql.expr.ExpressionParser;
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
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * SQL解析処理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlParserImpl implements SqlParser {
	/** SQLトークナイザ */
	private final SqlTokenizer tokenizer;

	/** ExpressionParser */
	private final ExpressionParser expressionParser;

	/** バインドコメントを実行時SQLに出力するかどうか */
	private final boolean outputBindComment;

	/** ノードのスタック */
	private final Stack<Node> nodeStack = new Stack<>();

	/** 終端文字削除用の正規表現 */
	private static final Pattern PATTERN = Pattern.compile(";$");

	/** 解析位置 */
	private int position;

	/**
	 * SqlParserImplのコンストラクタ
	 *
	 * @param sql パースするSQL
	 * @param expressionParser 評価式のパーサー
	 * @param removeTerminator 終端文字（;）を除去するかどうか
	 * @param outputBindComment バインド変数置換後にバインド変数のコメント文字列を出力するかどうか
	 */
	public SqlParserImpl(final String sql, final ExpressionParser expressionParser, final boolean removeTerminator,
			final boolean outputBindComment) {
		var s = sql.trim();
		if (removeTerminator) {
			s = PATTERN.matcher(s).replaceFirst("");
		}
		this.tokenizer = new SqlTokenizerImpl(s);
		this.expressionParser = expressionParser;
		this.outputBindComment = outputBindComment;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlParser#parse()
	 */
	@Override
	public ContextTransformer parse() {
		push(new ContainerNode(0, 0));
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
			parseElse(tokenizer.getToken().length());
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
		var sql = tokenizer.getToken();
		var node = peek();
		if ((node instanceof IfNode || node instanceof ElseNode) && node.getChildSize() == 0) {

			SqlTokenizer st = new SqlTokenizerImpl(sql);
			st.skipWhitespace();
			var token = st.skipToken();
			st.skipWhitespace();
			if ("AND".equalsIgnoreCase(token) || "OR".equalsIgnoreCase(token)) {
				node.addChild(new PrefixSqlNode(this.position, st.getBefore(), st.getAfter()));
			} else {
				node.addChild(new SqlNode(this.position, sql));
			}
		} else {
			node.addChild(new SqlNode(this.position, sql));
		}
		this.position = this.tokenizer.getPosition();
	}

	/**
	 * コメント解析
	 */
	protected void parseComment() {
		var comment = tokenizer.getToken();
		if (isTargetComment(comment)) {
			if (isIfComment(comment)) {
				parseIf();
			} else if (isElIfComment(comment)) {
				parseElIf();
			} else if (isElseComment(comment)) {
				parseElse(comment.length() + 4);
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
		var comment = tokenizer.getToken();
		var node = new SqlNode(Math.max(this.position - 2, 0), "/*" + comment + "*/");
		this.position = this.tokenizer.getPosition();
		peek().addChild(node);
	}

	/**
	 * IF文解析
	 */
	protected void parseIf() {
		var condition = tokenizer.getToken().substring(2);
		if (StringUtils.isBlank(condition)) {
			throw new IfConditionNotFoundRuntimeException();
		}
		var ifNode = new IfNode(expressionParser, Math.max(this.position - 2, 0), condition);
		this.position = this.tokenizer.getPosition();
		peek().addChild(ifNode);
		push(ifNode);
		parseEnd();
	}

	/**
	 * IF文解析
	 */
	protected void parseElIf() {
		var condition = tokenizer.getToken().substring(4);
		if (StringUtils.isBlank(condition)) {
			throw new IfConditionNotFoundRuntimeException();
		}
		var elifNode = new IfNode(expressionParser, Math.max(this.position - 2, 0), condition);
		this.position = this.tokenizer.getPosition();
		var ifNode = (IfNode) pop();
		ifNode.setElseIfNode(elifNode);
		push(elifNode);

		// parseEnd();
	}

	/**
	 * BEGIN文解析
	 */
	protected void parseBegin() {
		var beginNode = new BeginNode(Math.max(this.position - 2, 0));
		this.position = this.tokenizer.getPosition();
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
				this.position = this.tokenizer.getPosition();
				return;
			}
			parseToken();
		}
		throw new EndCommentNotFoundRuntimeException();
	}

	/**
	 * ELSE文解析
	 *
	 * @param length ELSE文の長さ
	 */
	protected void parseElse(final int length) {
		var parent = peek();
		if (!(parent instanceof IfNode)) {
			return;
		}
		var ifNode = (IfNode) pop();
		var elseNode = new ElseNode(Math.max(this.position - 2, 0), length);
		this.position = this.tokenizer.getPosition();
		ifNode.setElseNode(elseNode);
		push(elseNode);
		tokenizer.skipWhitespace();
	}

	/**
	 * バインド変数解析
	 */
	protected void parseCommentBindVariable() {
		var expr = tokenizer.getToken();
		var s = tokenizer.skipToken();
		if (s.startsWith("(") && s.endsWith(")")) {
			peek().addChild(new ParenBindVariableNode(expressionParser, Math.max(this.position - 2, 0), expr, s,
					outputBindComment));
		} else if (expr.startsWith("#")) {
			peek().addChild(new EmbeddedValueNode(expressionParser, Math.max(this.position - 2, 0), expr.substring(1),
					true, s));
		} else if (expr.startsWith("$")) {
			peek().addChild(
					new EmbeddedValueNode(expressionParser, Math.max(this.position - 2, 0), expr.substring(1), s));
		} else {
			peek().addChild(
					new BindVariableNode(expressionParser, Math.max(this.position - 2, 0), expr, s, outputBindComment));
		}
		this.position = this.tokenizer.getPosition();
	}

	/**
	 * バインド変数解析
	 */
	protected void parseBindVariable() {
		var expr = tokenizer.getToken();
		peek().addChild(new BindVariableNode(expressionParser, this.position, expr, null, outputBindComment));
		this.position = this.tokenizer.getPosition();
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
	 * @param node Nodeオブジェクト
	 */
	protected void push(final Node node) {
		nodeStack.push(node);
	}

	/**
	 * 解析対象コメント判定
	 *
	 * @param comment
	 * @return 解析対象の場合は<code>true</code>
	 */
	private static boolean isTargetComment(final String comment) {
		return comment != null
				&& comment.length() > 0
				&& (Character.isJavaIdentifierStart(comment.charAt(0)) || '#' == comment.charAt(0) || '(' == comment
						.charAt(0));
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
