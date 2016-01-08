using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;
using static Microsoft.CodeAnalysis.LanguageNames;

namespace Tutorial {
	[ExportCodeFixProvider(CSharp, Name = nameof(UsageOfClassInsteadOfInterfaceCodeFixProvider)), Shared]
	public class UsageOfClassInsteadOfInterfaceCodeFixProvider : CodeFixProvider {
		public override ImmutableArray<string> FixableDiagnosticIds
			=> ImmutableArray.Create(UsageOfClassInsteadOfInterfaceAnalyzer.DiagnosticId);

		public sealed override FixAllProvider GetFixAllProvider()
			=> WellKnownFixAllProviders.BatchFixer;

		public override async Task RegisterCodeFixesAsync(CodeFixContext context) {
			var diagnostic = context.Diagnostics.First();
			var interfaceName = diagnostic.Properties["interfaceName"];
			var className = diagnostic.Properties["className"];

			var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
			var typeSyntax = GetTypeSyntaxToReplace(diagnostic, root);

			context.RegisterCodeFix(
				CodeAction.Create(
					title: $"Replace usage of class '{className}' with the usage of interface '{interfaceName}'",
					createChangedDocument: cancellationToker => ReplaceUsage(
						context.Document,
						typeSyntax,
						interfaceName,
						diagnostic.Properties["interfaceNamespace"],
						cancellationToker),
					equivalenceKey: this.GetType().Name),
				diagnostic);
		}

		private static TypeSyntax GetTypeSyntaxToReplace(Diagnostic diagnostic, SyntaxNode root) {
			var diagnosticSpan = diagnostic.Location.SourceSpan;

			var nodeCandidates = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf();

			return nodeCandidates.OfType<VariableDeclarationSyntax>().FirstOrDefault()?.Type
				?? nodeCandidates.OfType<ParameterSyntax>().FirstOrDefault()?.Type;
		}

		private async Task<Document> ReplaceUsage(
			Document document, TypeSyntax type,
			string interfaceName, string interfaceNamespace,
			CancellationToken cancellationToken) {
			var syntaxRoot = await document.GetSyntaxRootAsync(cancellationToken) as CompilationUnitSyntax;
			var newRoot = syntaxRoot
				.ReplaceNode(type, IdentifierName(interfaceName))
				.AddUsings(
					UsingDirective(
						IdentifierName(interfaceNamespace))
							.WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation));

			return document.WithSyntaxRoot(newRoot);
		}
	}
}