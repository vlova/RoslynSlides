using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using static Microsoft.CodeAnalysis.LanguageNames;
using static Microsoft.CodeAnalysis.CSharp.SyntaxKind;
using System;

namespace Tutorial {
	[DiagnosticAnalyzer(CSharp)]
	public class UsageOfClassInsteadOfInterfaceAnalyzer : DiagnosticAnalyzer {
		public static string DiagnosticId = typeof(UsageOfClassInsteadOfInterfaceAnalyzer).Name;

		private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(
			DiagnosticId,
			title: "Usage of class instead of interface",
			messageFormat: "Usage of class '{0}' should be replaced to usage of interface '{1}'",
			category: "DI violation",
			description: "Variables and fields should be declared as having type of interface, not of specific class",
			defaultSeverity: DiagnosticSeverity.Warning, isEnabledByDefault: true);

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
			=> ImmutableArray.Create(Rule);

		public override void Initialize(AnalysisContext context) {
			context.RegisterSyntaxNodeAction(AnalyzeField, FieldDeclaration);
			context.RegisterSyntaxNodeAction(AnalyzeParameter, Parameter);
			context.RegisterSyntaxNodeAction(AnalyzeVariable, VariableDeclaration);
		}

		private void AnalyzeVariable(SyntaxNodeAnalysisContext context) {
			var node = context.Node as VariableDeclarationSyntax;
			AnalyzeTypeNode(context, node.GetLocation(), node.Type as IdentifierNameSyntax);
		}

		private void AnalyzeParameter(SyntaxNodeAnalysisContext context) {
			var node = context.Node as ParameterSyntax;
			AnalyzeTypeNode(context, node.GetLocation(), node.Type as IdentifierNameSyntax);
		}

		private void AnalyzeField(SyntaxNodeAnalysisContext context) {
			var node = context.Node as FieldDeclarationSyntax;
			AnalyzeTypeNode(context, node.Declaration.GetLocation(), node.Declaration.Type as IdentifierNameSyntax);
		}

		private static void AnalyzeTypeNode(
			SyntaxNodeAnalysisContext context,
			Location location,
			IdentifierNameSyntax typeNode) {
			if (typeNode == null || typeNode.IsVar) return;

			var classSymbol = context.SemanticModel.GetSymbolInfo(typeNode).Symbol as INamedTypeSymbol;
			if (classSymbol == null) return;
			if (classSymbol.TypeKind != TypeKind.Class) return;
			if (classSymbol.AllInterfaces.Count() != 1) return;
			if (!classSymbol.CanBeReferencedByName) return;

			var interfaceSymbol = classSymbol.AllInterfaces.Single();
			if (!interfaceSymbol.CanBeReferencedByName) return;

			context.ReportDiagnostic(
				Diagnostic.Create(
					Rule,
					location,
					properties: ImmutableDictionary.Create<string, string>()
						.Add("interfaceName", interfaceSymbol.Name)
						.Add("interfaceNamespace", interfaceSymbol.ContainingNamespace.Name)
						.Add("className", classSymbol.Name),
					messageArgs: new[] { classSymbol.Name, interfaceSymbol.Name }
			));
		}
	}
}
