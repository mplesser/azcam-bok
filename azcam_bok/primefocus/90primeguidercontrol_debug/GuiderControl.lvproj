<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="11008008">
	<Item Name="My Computer" Type="My Computer">
		<Property Name="NI.SortType" Type="Int">3</Property>
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="GuiderControl.vi" Type="VI" URL="../GuiderControl.vi"/>
		<Item Name="GalilServerCom.vi" Type="VI" URL="../GalilServerCom.vi"/>
		<Item Name="Dependencies" Type="Dependencies"/>
		<Item Name="Build Specifications" Type="Build">
			<Item Name="90PrimeGuiderControl" Type="EXE">
				<Property Name="App_INI_aliasGUID" Type="Str">{8E2CDDA7-55E1-4596-846F-9D2B5F9CE26F}</Property>
				<Property Name="App_INI_GUID" Type="Str">{BFE919BB-619E-41E0-8D69-7AEAEE8E129A}</Property>
				<Property Name="Bld_buildCacheID" Type="Str">{A3716280-E7A7-406A-911D-B03032C8C042}</Property>
				<Property Name="Bld_buildSpecName" Type="Str">90PrimeGuiderControl</Property>
				<Property Name="Bld_excludeLibraryItems" Type="Bool">true</Property>
				<Property Name="Bld_excludePolymorphicVIs" Type="Bool">true</Property>
				<Property Name="Bld_localDestDir" Type="Path">../90PrimeGuiderControl</Property>
				<Property Name="Bld_localDestDirType" Type="Str">relativeToCommon</Property>
				<Property Name="Bld_modifyLibraryFile" Type="Bool">true</Property>
				<Property Name="Bld_previewCacheID" Type="Str">{5365122B-0C07-4C93-89E5-0116A2DF6661}</Property>
				<Property Name="Destination[0].destName" Type="Str">90PrimeGuiderControl.exe</Property>
				<Property Name="Destination[0].path" Type="Path">../90PrimeGuiderControl/90PrimeGuiderControl.exe</Property>
				<Property Name="Destination[0].type" Type="Str">App</Property>
				<Property Name="Destination[1].destName" Type="Str">Support Directory</Property>
				<Property Name="Destination[1].path" Type="Path">../90PrimeGuiderControl/data</Property>
				<Property Name="DestinationCount" Type="Int">2</Property>
				<Property Name="Source[0].itemID" Type="Str">{96B9BBD6-9E07-4CFC-A7A9-FA544E348F33}</Property>
				<Property Name="Source[0].type" Type="Str">Container</Property>
				<Property Name="Source[1].destinationIndex" Type="Int">0</Property>
				<Property Name="Source[1].itemID" Type="Ref">/My Computer/GuiderControl.vi</Property>
				<Property Name="Source[1].sourceInclusion" Type="Str">TopLevel</Property>
				<Property Name="Source[1].type" Type="Str">VI</Property>
				<Property Name="SourceCount" Type="Int">2</Property>
				<Property Name="TgtF_companyName" Type="Str">University of Arizona</Property>
				<Property Name="TgtF_fileDescription" Type="Str">90PrimeGuiderControl</Property>
				<Property Name="TgtF_fileVersion.major" Type="Int">1</Property>
				<Property Name="TgtF_internalName" Type="Str">90PrimeGuiderControl</Property>
				<Property Name="TgtF_legalCopyright" Type="Str">Copyright © 2008 University of Arizona</Property>
				<Property Name="TgtF_productName" Type="Str">90PrimeGuiderControl</Property>
				<Property Name="TgtF_targetfileGUID" Type="Str">{3A47219B-3A34-4AAD-9F23-843AB0D37CB8}</Property>
				<Property Name="TgtF_targetfileName" Type="Str">90PrimeGuiderControl.exe</Property>
			</Item>
		</Item>
	</Item>
</Project>
