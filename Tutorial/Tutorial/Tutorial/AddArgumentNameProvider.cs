using System.Composition;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using Microsoft.CodeAnalysis.Formatting;
using static Microsoft.CodeAnalysis.LanguageNames;
using Microsoft.CodeAnalysis.Simplification;
using static Microsoft.CodeAnalysis.SymbolKind;

namespace Tutorial {
	[ExportCodeRefactoringProvider(nameof(AddArgumentNameProvider), CSharp), Shared]
	internal class AddArgumentNameProvider : CodeRefactoringProvider {
		public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
			var syntaxRoot = await context.Document.GetSyntaxRootAsync(context.CancellationToken);
			var argument = syntaxRoot.FindNode(context.Span).FirstAncestorOrSelf<ArgumentSyntax>();
			if (argument == null) return;

			var argumentList = argument.Parent as ArgumentListSyntax;
			var calleeMethod = argumentList.Parent as ExpressionSyntax;
			var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
			var methodSymbol = semanticModel.GetSymbolInfo(calleeMethod).Symbol as IMethodSymbol;
			if (methodSymbol == null || methodSymbol.Kind != Method) return;

			context.RegisterRefactoring(
				CodeAction.Create(
					title: "Add argument name",
					createChangedDocument: cancellationToken => ComputeRoot(context.Document, syntaxRoot, argument, methodSymbol),
					equivalenceKey: typeof(AddArgumentNameProvider).Name
			));
		}

		private Task<Document> ComputeRoot(
			Document document, SyntaxNode root,
			ArgumentSyntax argument, IMethodSymbol methodSymbol) {
			var providenArgumentsList = argument.Parent as ArgumentListSyntax;
			var providenArguments = providenArgumentsList.Arguments;
			var argumentIndex = providenArguments.IndexOf(argument);

			var newArguments = Enumerable.Concat(
				providenArguments.Take(argumentIndex),
				providenArguments.Skip(argumentIndex)
					.Select((arg, index) => {
						var parameterName = methodSymbol.Parameters[index].Name;
						return arg
							.WithNameColon(NameColon(IdentifierName(parameterName)))
							.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
					}));

			var newArgumentList = providenArgumentsList.WithArguments(SeparatedList(newArguments));

			var newRoot = root.ReplaceNode(providenArgumentsList, newArgumentList);
			return Task.FromResult(document.WithSyntaxRoot(newRoot));
		}
	}
}
