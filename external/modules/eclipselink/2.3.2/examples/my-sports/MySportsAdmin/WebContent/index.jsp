<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>MySports Admin</title>
<link rel="stylesheet" type="text/css" href="default.css" />
</head>
<body>
	<h1>MySports Admin</h1>
	<h2>Provision New League</h2>
	<div align="center">
		<form action="rest/league" method="post">
			<table width="500" border="1" class="pretty">
				<tr>
					<th colspan="2">Create New league</th>
				</tr>
				<tr>
					<td>ID:</td>
					<td><input name="league-id">
					</td>
				</tr>
				<tr>
					<td>Name:</td>
					<td><input name="league-name">
					</td>
				</tr>
				<tr>
					<td>Colour Scheme:</td>
					<td><select name="colour-scheme" >
							<option	value="default">Default</option>
							<option value="red">Red</option>
							<option value="blue">Blue</option>
							<option value="black">Black</option>
							<option value="green">Green</option>
					</select>
					</td>
				</tr>
			</table>
			<button type="submit">Create</button>
		</form>
	</div>
	<h2>Define New Player Extension</h2>
	<div align="center">
		<form action="rest/extensions/Player" method="post">
			<table width="500" border="1" class="pretty">
				<tr>
					<th colspan="2">Create New Player Extension</th>
				</tr>
				<tr>
					<td>League ID:</td>
					<td><input name="league-id">
					</td>
				</tr>
				<tr>
					<td>Name:</td>
					<td><input name="attribute-name">
					</td>
				</tr>
				<tr>
					<td>Type:</td>
					<td><input name="attribute-type" value="java.lang.String">
					</td>
				</tr>
				<tr>
					<td>Column:</td>
					<td><input name="column-name" value="flex_3">
					</td>
				</tr>
				<tr>
					<td>XML Path:</td>
					<td><input name="xml-path" value="">
					</td>
				</tr>
			</table>
			<button type="submit">Create</button>
		</form>
	</div>
	<h2>
		<a href="/MySports">My Sports</a>
	</h2>
	<h2>Queries</h2>
	<ul>
		<li><a href="rest/league">List Leagues</a>
		</li>
	</ul>
</body>
</html>